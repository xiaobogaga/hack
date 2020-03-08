package compiler

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

type Parser struct {
	currentTokenPos int
	currentTokens   []*Token
}

func (parser *Parser) Parse(filePath string) (classAsts []*ClassAst, err error) {
	files, err := ioutil.ReadDir(filePath)
	if err != nil {
		return nil, err
	}

	for _, file := range files {
		// Skip not-jack file.
		if !isJackFile(file) {
			continue
		}
		classAst, err := parser.ParseFile(file.Name())
		if err != nil {
			return nil, err
		}
		classAsts = append(classAsts, classAst)
	}
	return
}

func isJackFile(fileInfo os.FileInfo) bool {
	fileName := fileInfo.Name()
	return fileName[len(fileName)-5:] == ".jack"
}

func (parser *Parser) ParseFile(fileName string) (*ClassAst, error) {
	tokenizer := &Tokenizer{}
	rd, err := os.Open(fileName)
	if err != nil {
		return nil, err
	}
	tokens, err := tokenizer.Tokenize(rd)
	if err != nil {
		return nil, err
	}
	return parser.ParseClassDeclaration(tokens)
}

// class Identifier {
//
// }
func (parser *Parser) ParseClassDeclaration(tokens []*Token) (*ClassAst, error) {
	_, match := parser.expectToken(ClassTP, true)
	if !match {
		return nil, parser.makeError()
	}

	classNameToken, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError()
	}
	className := classNameToken.Value(IdentifierTP).(string)

	classVariableAst, classFuncOrMethodAst, err := parser.ParseClassBody()
	if err != nil {
		return nil, err
	}

	return &ClassAst{
		className:         className,
		classVariables:    classVariableAst,
		classFuncOrMethod: classFuncOrMethodAst,
	}, nil
}

// ClassBody can contains variable or method declaration.
// {
//    varDefs
//    methodDefs
// }
func (parser *Parser) ParseClassBody() (vars []*ClassVariableAst, methods []*ClassFuncOrMethodAst, err error) {
	_, match := parser.expectToken(LeftBraceTP, true)
	if !match {
		return nil, nil, parser.makeError()
	}
	var variable *ClassVariableAst
	var method *ClassFuncOrMethodAst
	for parser.currentTokenPos < len(parser.currentTokens) {
		token := parser.currentTokens[parser.currentTokenPos]
		switch token.tp {
		case StaticTP, FieldTP:
			variable, err = parser.ParseVariableDeclaration()
		case ConstructorTP, FunctionTP, MethodTP:
			method, err = parser.ParseFuncOrMethodDeclaration()
		case RightBraceTP:
			break
		default:
			err = parser.makeError()
		}
		if err != nil {
			return nil, nil, err
		}
		if variable != nil {
			vars = append(vars, variable)
			variable = nil
		}
		if method != nil {
			methods = append(methods, method)
			method = nil
		}
	}
	return
}

// Var declaration like: [static|field] [boolean|char|int|className] varName [,varName]* ;
func (parser *Parser) ParseVariableDeclaration() (vars *ClassVariableAst, err error) {
	token := parser.currentTokens[parser.currentTokenPos]
	fieldTp := ObjectFieldType
	switch token.tp {
	case FieldTP:
		fieldTp = ObjectFieldType
	case StaticTP:
		fieldTp = ClassFieldType
	}
	varType, className, err := parser.ParseVariableType()
	if err != nil {
		return nil, err
	}
	// Must follow a varName.
	_, match := parser.expectToken(IdentifierTP, false)
	if !match {
		return nil, parser.makeError()
	}
	var varNames []string
	for parser.currentTokenPos < len(parser.currentTokens) {
		varNameToken, match := parser.expectToken(IdentifierTP, true)
		if !match {
			return nil, parser.makeError()
		}
		varName := varNameToken.Value(IdentifierTP).(string)
		varNames = append(varNames, varName)
		token := parser.currentTokens[parser.currentTokenPos]
		if token.tp == CommaTP {
			continue
		}
		if token.tp == SemiColonTP {
			break
		}
	}
	if parser.currentTokenPos >= len(parser.currentTokens) ||
		parser.currentTokens[parser.currentTokenPos].tp != SemiColonTP {
		return nil, parser.makeError()
	}
	return &ClassVariableAst{
		VariableNames: varNames,
		FieldTP:       fieldTp,
		VariableType:  varType,
		ClassName:     className,
	}, nil
}

func (parser *Parser) ParseVariableType() (v VariableType, className string, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		err = parser.makeError()
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntTP:
		v = IntVariableType
	case CharTP:
		v = CharVariableType
	case BooleanTP:
		v = BooleanVariableType
	case IdentifierTP:
		v = ClassVariableType
		className = token.content
	default:
		err = parser.makeError()
	}
	return
}

// Method Declaration:
// [constructor|function|method] [void|int|boolean|char] methodName ( {[int|boolean|char|className] varName,...}* ) {
//   MethodBody
// }
//
// MethodBody:
//  * varDesc: var [int|boolean|char|className] varName [, varName, ...]* ;
//  * statements: LetStatement| IfStatement | WhileStatement | DoStatement | ReturnStatement
func (parser *Parser) ParseFuncOrMethodDeclaration() (*ClassFuncOrMethodAst, error) {
	funcTp, err := parser.parseFuncType()
	if err != nil {
		return nil, err
	}

	returnTp, retClassName, err := parser.parseFuncReturnType()
	if err != nil {
		return nil, err
	}

	methodNameToken, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError()
	}
	methodName := methodNameToken.Value(IdentifierTP).(string)

	paramList, err := parser.parseFuncParamList()
	if err != nil {
		return nil, err
	}

	methodBody, err := parser.parseFuncBody()
	if err != nil {
		return nil, err
	}

	return &ClassFuncOrMethodAst{
		FuncTP:          funcTp,
		FuncName:        methodName,
		ReturnTP:        returnTp,
		ReturnClassName: retClassName,
		Params:          paramList,
		FuncBody:        methodBody,
	}, nil
}

func (parser *Parser) parseFuncType() (funcTP FuncType, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		err = parser.makeError()
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case ConstructorTP:
		funcTP = ClassConstructorType
	case FunctionTP:
		funcTP = ClassFuncType
	case MethodTP:
		funcTP = ClassMethodType
	default:
		err = parser.makeError()
	}
	return
}

func (parser *Parser) parseFuncReturnType() (retTP ReturnType, retClassName string, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		err = parser.makeError()
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case VoidTP:
		retTP = VoidReturnType
	case IntTP:
		retTP = IntReturnType
	case CharTP:
		retTP = CharReturnType
	case BooleanTP:
		retTP = BooleanReturnType
	case IdentifierTP:
		retTP = ClassReturnType
		retClassName = token.content
	default:
		err = parser.makeError()
	}
	return
}

func (parser *Parser) parseFuncParamList() (ast []*FuncParamAst, err error) {
	_, match := parser.expectToken(LeftParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	// Empty param list.
	_, match = parser.expectToken(RightParentThesesTP, false)
	if match {
		parser.stepForward()
		return nil, nil
	}

	for parser.currentTokenPos < len(parser.currentTokens) {
		varType, varName, varClassName, err := parser.parseFuncVarTypeName()
		if err != nil {
			return nil, err
		}
		ast = append(ast, &FuncParamAst{ParamTP: varType, ParamName: varName, ParamClassName: varClassName})
		_, match = parser.expectToken(CommaTP, false)
		if match {
			parser.stepForward()
			break
		}
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return
}

func (parser *Parser) parseFuncVarTypeName() (paramType ParamType, paramName, paramClassName string, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		err = parser.makeError()
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntTP:
		paramType = IntParamType
	case CharTP:
		paramType = CharParamType
	case BooleanTP:
		paramType = BooleanParamType
	case IdentifierTP:
		paramType = ClassParamType
		paramClassName = token.content
	}
	if parser.currentTokenPos >= len(parser.currentTokens) {
		err = parser.makeError()
		return
	}
	if parser.currentTokens[parser.currentTokenPos].tp != IdentifierTP {
		err = parser.makeError()
		return
	}
	paramClassName = parser.currentTokens[parser.currentTokenPos].content
	return
}

func (parser *Parser) stepForward() {
	parser.currentTokenPos++
}

// {
//    statements
// }
func (parser *Parser) parseFuncBody() (ast []*StatementAst, err error) {
	_, match := parser.expectToken(LeftBraceTP, true)
	if !match {
		return nil, parser.makeError()
	}
	// If it's empty method
	_, match = parser.expectToken(RightBraceTP, false)
	if match {
		parser.stepForward()
		return nil, nil
	}
	ast, err = parser.parseStatements()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(RightBraceTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return
}

func (parser *Parser) parseStatements() (stms []*StatementAst, err error) {
	for parser.currentTokenPos < len(parser.currentTokens) {
		statement, err := parser.parseStatement()
		if err != nil {
			return nil, err
		}
		stms = append(stms, statement)
		_, match := parser.expectToken(RightBraceTP, false)
		if match {
			break
		}
	}
	return
}

func (parser *Parser) parseStatement() (stm *StatementAst, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case VarTP:
		stm, err = parser.parseVarDeclareStatement()
	case LetTP:
		stm, err = parser.parseLetStatement()
	case DoTp:
		stm, err = parser.parseDoStatement()
	case IfTP:
		stm, err = parser.parseIfStatement()
	case WhileTP:
		stm, err = parser.parseWhileStatement()
	case ReturnTP:
		stm, err = parser.parseReturnStatement()
	default:
		err = parser.makeError()
	}
	return
}

func (parser *Parser) parseVarDeclareStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(VarTP, true)
	if !match {
		return nil, parser.makeError()
	}
	varType, varClassName, err := parser.ParseVariableType()
	if err != nil {
		return nil, err
	}
	// Must follow a varName.
	_, match = parser.expectToken(IdentifierTP, false)
	if !match {
		return nil, parser.makeError()
	}
	var varNames []string
	for parser.currentTokenPos < len(parser.currentTokens) {
		varNameToken, match := parser.expectToken(IdentifierTP, true)
		if !match {
			return nil, parser.makeError()
		}
		varName := varNameToken.Value(IdentifierTP).(string)
		varNames = append(varNames, varName)
		token := parser.currentTokens[parser.currentTokenPos]
		if token.tp == CommaTP {
			continue
		}
		if token.tp == SemiColonTP {
			break
		}
	}
	if parser.currentTokenPos >= len(parser.currentTokens) ||
		parser.currentTokens[parser.currentTokenPos].tp != SemiColonTP {
		return nil, parser.makeError()
	}
	return &StatementAst{
		StatementTP: VariableDeclareTP,
		Statement: &VarDeclareAst{
			VarNames:         varNames,
			VarType:          varType,
			VarTypeClassName: varClassName,
		},
	}, nil
}

func (parser *Parser) parseLetStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(LetTP, true)
	if !match {
		return nil, parser.makeError()
	}

	letVariable, err := parser.parseLetVariableAst()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(EqualTP, true)
	if !match {
		return nil, parser.makeError()
	}

	valueExpression, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, err
	}

	return &StatementAst{
		StatementTP: LetStatementTP,
		Statement: &LetStatementAst{
			LetVariable: letVariable,
			Value:       valueExpression,
		},
	}, nil
}

func (parser *Parser) parseExpression() (ast *ExpressionAst, err error) {
	leftExprTerm, err := parser.parseExpressionTerm()
	if err != nil {
		return nil, err
	}

	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}

	token := parser.currentTokens[parser.currentTokenPos]
	if token.tp == SemiColonTP {
		return &ExpressionAst{LeftExpr: leftExprTerm}, nil
	}

	op, err := parser.parseOpAst()
	if err != nil {
		return nil, err
	}

	rightExprTerm, err := parser.parseExpressionTerm()
	if err != nil {
		return nil, err
	}

	return &ExpressionAst{
		LeftExpr:  leftExprTerm,
		Op:        op,
		RightExpr: rightExprTerm,
	}, nil
}

func (parser *Parser) parseExpressionTerm() (expr *ExpressionTerm, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntTP, StringTP, TrueTP, FalseTP, NullTP, ThisTP:
		expr, err = parser.parseConstantExpressionTerm()
	// When it's identifier, it can be a SubRoutine call or
	// VarName expression like: i
	case IdentifierTP:
		expr, err = parser.parseSubRoutineCallExpressionOrVarExpressionTerm()
	case LeftParentThesesTP:
		expr, err = parser.parseSubExpressionTerm()
	// An unary operation for negative.
	case MinusTP, BooleanNegativeTP:
		expr, err = parser.parseNegationExpressionTerm()
	default:
		err = parser.makeError()
	}
	return
}

func (parser *Parser) parseConstantExpressionTerm() (term *ExpressionTerm, err error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	term = new(ExpressionTerm)
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntTP:
		term.Type = IntegerConstantTermType
		term.Value, _ = strconv.Atoi(token.content)
	case StringTP:
		term.Type = StringConstantTermType
		term.Value = token.content
	case TrueTP:
		term.Type = KeyWordConstantTrueTermType
	case FalseTP:
		term.Type = KeyWordConstantFalseTermType
	case NullTP:
		term.Type = KeyWordConstantNullTermType
	case ThisTP:
		term.Type = KeyWordConstantThisTermType
	default:
		return nil, parser.makeError()
	}
	return
}

// Could be varName|varName[expression]|varName.funcName|varname[expression].funcName.
func (parser *Parser) parseSubRoutineCallExpressionOrVarExpressionTerm() (*ExpressionTerm, error) {
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError()
	}
	expr := new(ExpressionTerm)
	if parser.currentTokenPos >= len(parser.currentTokens) {
		expr.Value, expr.Type = token.content, VarNameExpressionTermType
		return expr, nil
	}
	token2 := parser.currentTokens[parser.currentTokenPos]
	switch token2.tp {
	case LeftSquareBracketTP:
		// Should be an array index.
		exprAst, err := parser.parseArrayIndexExpression()
		if err != nil {
			return nil, err
		}
		expr.Value, expr.Type = exprAst, ArrayIndexExpressionTermType
	case DotTP, LeftParentThesesTP:
		// Should be a funcCall
		callAst, err := parser.parseFuncCall()
		if err != nil {
			return nil, err
		}
		expr.Value, expr.Type = callAst, SubRoutineCallTermType
	default:
		expr.Value, expr.Type = token.content, VarNameExpressionTermType
	}
	return expr, nil
}

func (parser *Parser) parseSubExpressionTerm() (*ExpressionTerm, error) {
	_, match := parser.expectToken(LeftParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	expr, err := parser.parseExpressionTerm()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return &ExpressionTerm{
		Type:  SubExpressionTermType,
		Value: expr,
	}, nil
}

func (parser *Parser) parseNegationExpressionTerm() (*ExpressionTerm, error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	token := parser.currentTokens[parser.currentTokenPos]
	op := new(OpAst)
	op.OpTP = UnaryOPTP
	switch token.tp {
	case BooleanNegativeTP:
		op.Op = BooleanNegationOpTP
	case MinusTP:
		op.Op = NegationOpTP
	default:
		return nil, parser.makeError()
	}
	parser.stepForward()
	exprTerm, err := parser.parseExpressionTerm()
	if err != nil {
		return nil, err
	}
	return &ExpressionTerm{
		UnaryOp: op,
		Type:    UnaryTermExpressionTermType,
		Value:   exprTerm,
	}, nil
}

func (parser *Parser) parseOpAst() (*OpAst, error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	token := parser.currentTokens[parser.currentTokenPos]
	op := new(OpAst)
	op.OpTP = BinaryOPTP
	switch token.tp {
	case AddTP:
		op.Op = AddOpTP
	case MinusTP:
		op.Op = MinusOpTP
	case MultiplyTP:
		op.Op = MultipleOpTP
	case DivideTP:
		op.Op = DivideOpTP
	case AndTP:
		op.Op = AndOpTP
	case OrTP:
		op.Op = OrOpTP
	case GreaterTP:
		op.Op = GreaterOpTP
	case LessTP:
		op.Op = LessOpTP
	case EqualTP:
		op.Op = EqualOpTp
	default:
		return nil, parser.makeError()
	}
	return op, nil
}

func (parser *Parser) parseLetVariableAst() (*VariableAst, error) {
	if parser.currentTokenPos >= len(parser.currentTokens) {
		return nil, parser.makeError()
	}
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError()
	}
	varName := token.content
	var arrayIndexExpression *ExpressionAst
	var err error
	_, match = parser.expectToken(LeftSquareBracketTP, false)
	if match {
		arrayIndexExpression, err = parser.parseArrayIndexExpression()
	}
	if err != nil {
		return nil, err
	}
	return &VariableAst{
		VarName:    varName,
		ArrayIndex: arrayIndexExpression,
	}, nil
}

func (parser *Parser) parseArrayIndexExpression() (*ExpressionAst, error) {
	_, match := parser.expectToken(LeftSquareBracketTP, true)
	if !match {
		return nil, parser.makeError()
	}
	arrayIndexExpression, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightSquareBracketTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return arrayIndexExpression, nil
}

func (parser *Parser) parseDoStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(DoTp, true)
	if !match {
		return nil, parser.makeError()
	}
	funcCall, err := parser.parseFuncCall()
	if err != nil {
		return nil, err
	}
	return &StatementAst{
		StatementTP: DoStatementTP,
		Statement:   &DoStatementAst{Call: funcCall},
	}, nil
}

func (parser *Parser) parseFuncCall() (*CallAst, error) {
	funcProvider, funcName, err := parser.parseFuncCallProvider()
	if err != nil {
		return nil, err
	}
	_, match := parser.expectToken(LeftParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	expressions, err := parser.parseExpressions()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return &CallAst{
		FuncProvider: funcProvider,
		FuncName:     funcName,
		Params:       expressions,
	}, nil
}

func (parser *Parser) parseFuncCallProvider() (funcProvider string, funcName string, err error) {
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		err = parser.makeError()
		return
	}
	funcName = token.content
	_, match = parser.expectToken(DotTP, false)
	if !match {
		return
	}
	parser.stepForward()
	funcNameToken, match := parser.expectToken(IdentifierTP, true)
	if !match {
		err = parser.makeError()
		return
	}
	funcProvider = funcName
	funcName = funcNameToken.content
	return
}

func (parser *Parser) parseIfStatement() (stm *StatementAst, err error) {
	match := parser.expectTokens(IfTP, LeftBraceTP)
	if !match {
		return nil, parser.makeError()
	}
	condition, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	match = parser.expectTokens(RightBraceTP, LeftBraceTP)
	if !match {
		return nil, parser.makeError()
	}
	ifTrueStatement, err := parser.parseStatements()
	if err != nil {
		return nil, err
	}
	elseStatement, err := parser.parseElseStatement()
	if err != nil {
		return nil, err
	}
	return &StatementAst{
		StatementTP: IfStatementTP,
		Statement: &IfStatementAst{
			Condition:        condition,
			IfTrueStatements: ifTrueStatement,
			ElseStatements:   elseStatement,
		},
	}, nil
}

func (parser *Parser) parseElseStatement() ([]*StatementAst, error) {
	_, match := parser.expectToken(ElseTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return parser.parseStatements()
}

func (parser *Parser) parseWhileStatement() (stm *StatementAst, err error) {
	match := parser.expectTokens(WhileTP, LeftParentThesesTP)
	if !match {
		return nil, parser.makeError()
	}
	condition, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	match = parser.expectTokens(RightParentThesesTP, LeftBraceTP)
	if !match {
		return nil, parser.makeError()
	}
	statements, err := parser.parseStatements()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightBraceTP, true)
	if !match {
		return nil, parser.makeError()
	}
	return &StatementAst{
		StatementTP: WhileStatementTP,
		Statement: &WhileStatementAst{
			Condition:  condition,
			Statements: statements,
		},
	}, nil
}

func (parser *Parser) parseReturnStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(ReturnTP, true)
	if !match {
		return nil, parser.makeError()
	}
	expressions, err := parser.parseExpressions()
	if err != nil {
		return nil, err
	}
	return &StatementAst{
		StatementTP: ReturnStatementTP,
		Statement:   &ReturnStatementAst{Return: expressions},
	}, nil
}

func (parser *Parser) parseExpressions() (exprs []*ExpressionAst, err error) {
	for parser.currentTokenPos < len(parser.currentTokens) {
		expression, err := parser.parseExpression()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expression)
		_, match := parser.expectToken(CommaTP, false)
		if !match {
			break
		}
		parser.stepForward()
	}
	return
}

func (parser *Parser) expectTokens(expectedTokenTPs ...TokenType) bool {
	for _, token := range expectedTokenTPs {
		_, ok := parser.expectToken(token, true)
		if !ok {
			return false
		}
	}
	return true
}

func (parser *Parser) expectToken(expectedTokenTp TokenType, walk bool) (*Token, bool) {
	if parser.currentTokenPos >= len(parser.currentTokens) || parser.currentTokens[parser.currentTokenPos].tp !=
		expectedTokenTp {
		return nil, false
	}
	token := parser.currentTokens[parser.currentTokenPos]
	if walk {
		parser.currentTokenPos++
	}
	return token, true
}

func (parser *Parser) makeError() error {
	currentToken := parser.currentTokens[parser.currentTokenPos]
	return errors.New(fmt.Sprintf("syntax error near %s at line %d", currentToken.content,
		currentToken.line))
}

package compiler

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
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
		if !isJackFile(file.Name()) {
			continue
		}
		parser.reset()
		classAst, err := parser.ParseFile(file.Name())
		if err != nil {
			return nil, err
		}
		classAsts = append(classAsts, classAst)
	}
	return
}

func (parser *Parser) reset() {
	parser.currentTokenPos, parser.currentTokens = 0, nil
}

func isJackFile(fileName string) bool {
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
	parser.currentTokens = tokens
	return parser.ParseClassDeclaration()
}

// class Identifier {
//
// }
func (parser *Parser) ParseClassDeclaration() (*ClassAst, error) {
	_, match := parser.expectToken(ClassTP, true)
	if !match {
		return nil, parser.makeError(false)
	}

	classNameToken, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	className := classNameToken.Value(IdentifierTP).(string)

	classVariableAst, classFuncOrMethodAst, err := parser.ParseClassBody(className)
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
func (parser *Parser) ParseClassBody(className string) (vars []*ClassVariableAst, methods []*ClassFuncOrMethodAst, err error) {
	_, match := parser.expectToken(LeftBraceTP, true)
	if !match {
		return nil, nil, parser.makeError(false)
	}
	var variables []*ClassVariableAst
	var method *ClassFuncOrMethodAst
	for parser.hasRemainTokens() {
		token, _ := parser.getCurrentToken()
		switch token.tp {
		case StaticTP, FieldTP:
			variables, err = parser.ParseVariableDeclaration()
		case ConstructorTP, FunctionTP, MethodTP:
			method, err = parser.ParseFuncOrMethodDeclaration(className)
		case RightBraceTP:
			parser.stepForward()
			break
		default:
			err = parser.makeError(true)
		}
		if err != nil {
			return nil, nil, err
		}
		if len(variables) > 0 {
			vars = append(vars, variables...)
			variables = nil
		}
		if method != nil {
			methods = append(methods, method)
			method = nil
		}
	}
	// If still has remain tokens, return err
	if parser.hasRemainTokens() {
		return nil, nil, parser.makeError(true)
	}
	return
}

// Var declaration like: [static|field] [boolean|char|int|className] varName [,varName]* ;
func (parser *Parser) ParseVariableDeclaration() (vars []*ClassVariableAst, err error) {
	token, err := parser.getCurrentToken()
	if err != nil {
		return nil, err
	}
	fieldTp := ObjectFieldType
	switch token.tp {
	case FieldTP:
		fieldTp = ObjectFieldType
	case StaticTP:
		fieldTp = ClassFieldType
	default:
		return nil, parser.makeError(true)
	}
	parser.stepForward()
	varType, err := parser.ParseVariableType()
	if err != nil {
		return nil, err
	}
	// Must follow a varName.
	_, match := parser.expectToken(IdentifierTP, false)
	if !match {
		return nil, parser.makeError(true)
	}
	for parser.hasRemainTokens() {
		varNameToken, match := parser.expectToken(IdentifierTP, true)
		if !match {
			return nil, parser.makeError(false)
		}
		varName := varNameToken.Value(IdentifierTP).(string)
		vars = append(vars, &ClassVariableAst{
			VariableName: varName,
			FieldTP:      fieldTp,
			VariableType: varType,
		})
		token, err := parser.getCurrentToken()
		if err != nil {
			return nil, err
		}
		if token.tp == CommaTP {
			parser.stepForward()
			continue
		}
		break
	}
	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return
}

func (parser *Parser) getCurrentToken() (*Token, error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(true)
	}
	return parser.currentTokens[parser.currentTokenPos], nil
}

func (parser *Parser) ParseVariableType() (v VariableType, err error) {
	if !parser.hasRemainTokens() {
		err = parser.makeError(true)
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntTP:
		v.TP = IntVariableType
	case CharTP:
		v.TP = CharVariableType
	case BooleanTP:
		v.TP = BooleanVariableType
	case IdentifierTP:
		v.TP, v.Name = ClassVariableType, token.content
	default:
		err = parser.makeError(true)
	}
	if err != nil {
		return
	}
	parser.stepForward()
	return
}

// Method Declaration:
// [constructor|function|method] [void|int|boolean|char|className] methodName ( {[int|boolean|char|className] varName,...}* ) {
//   MethodBody
// }
//
// MethodBody:
//  * varDesc: var [int|boolean|char|className] varName [, varName, ...]* ;
//  * statements: LetStatement| IfStatement | WhileStatement | DoStatement | ReturnStatement
func (parser *Parser) ParseFuncOrMethodDeclaration(className string) (*ClassFuncOrMethodAst, error) {
	funcTp, err := parser.parseFuncType()
	if err != nil {
		return nil, err
	}

	returnTp, err := parser.parseFuncReturnType()
	if err != nil {
		return nil, err
	}

	methodNameToken, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		FuncTP:   funcTp,
		FuncName: methodName,
		ReturnTP: returnTp,
		Params:   paramList,
		FuncBody: methodBody,
	}, nil
}

func (parser *Parser) parseFuncType() (funcTP FuncType, err error) {
	if !parser.hasRemainTokens() {
		err = parser.makeError(true)
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
		err = parser.makeError(true)
	}
	parser.stepForward()
	return
}

func (parser *Parser) parseFuncReturnType() (retTP VariableType, err error) {
	if !parser.hasRemainTokens() {
		err = parser.makeError(true)
		return
	}
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case VoidTP:
		retTP.TP = VoidVariableType
	case IntTP:
		retTP.TP = IntVariableType
	case CharTP:
		retTP.TP = CharVariableType
	case BooleanTP:
		retTP.TP = BooleanVariableType
	case IdentifierTP:
		retTP.TP, retTP.Name = ClassVariableType, token.content
	default:
		err = parser.makeError(true)
	}
	parser.stepForward()
	return
}

func (parser *Parser) parseFuncParamList() (ast []*FuncParamAst, err error) {
	_, match := parser.expectToken(LeftParentThesesTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	// Empty param list.
	_, match = parser.expectToken(RightParentThesesTP, false)
	if match {
		parser.stepForward()
		return nil, nil
	}

	for parser.hasRemainTokens() {
		varType, varName, err := parser.parseFuncVarTypeName()
		if err != nil {
			return nil, err
		}
		ast = append(ast, &FuncParamAst{ParamTP: varType, ParamName: varName})
		_, match = parser.expectToken(CommaTP, false)
		if !match {
			break
		}
		parser.stepForward()
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return
}

func (parser *Parser) parseFuncVarTypeName() (paramType VariableType, paramName string, err error) {
	if !parser.hasRemainTokens() {
		err = parser.makeError(false)
		return
	}
	token, _ := parser.getCurrentToken()
	switch token.tp {
	case IntTP:
		paramType.TP = IntVariableType
	case CharTP:
		paramType.TP = CharVariableType
	case BooleanTP:
		paramType.TP = BooleanVariableType
	case IdentifierTP:
		paramType.TP = ClassVariableType
		paramType.Name = token.content
	default:
		err = parser.makeError(true)
		return
	}
	parser.stepForward()
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		err = parser.makeError(false)
		return
	}
	paramName = token.content
	return
}

func (parser *Parser) stepForward() {
	parser.currentTokenPos++
}

func (parser *Parser) hasRemainTokens() bool {
	return parser.currentTokenPos < len(parser.currentTokens)
}

// {
//    statements
// }
func (parser *Parser) parseFuncBody() (ast []*StatementAst, err error) {
	_, match := parser.expectToken(LeftBraceTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		return nil, parser.makeError(false)
	}
	return
}

func (parser *Parser) parseStatements() (stms []*StatementAst, err error) {
	for parser.hasRemainTokens() {
		_, match := parser.expectToken(RightBraceTP, false)
		if match {
			break
		}
		statement, err := parser.parseStatement()
		if err != nil {
			return nil, err
		}
		stms = append(stms, statement...)
	}
	return
}

func (parser *Parser) parseStatement() (stms []*StatementAst, err error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(true)
	}
	token, _ := parser.getCurrentToken()
	var stm *StatementAst
	switch token.tp {
	case VarTP:
		stms, err = parser.parseVarDeclareStatement()
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
		err = parser.makeError(true)
	}
	if stm != nil {
		stms = append(stms, stm)
	}
	return
}

// var int a, b;
func (parser *Parser) parseVarDeclareStatement() (stms []*StatementAst, err error) {
	_, match := parser.expectToken(VarTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	varType, err := parser.ParseVariableType()
	if err != nil {
		return nil, err
	}
	// Must follow a varName.
	_, match = parser.expectToken(IdentifierTP, false)
	if !match {
		return nil, parser.makeError(true)
	}
	for parser.hasRemainTokens() {
		varNameToken, match := parser.expectToken(IdentifierTP, true)
		if !match {
			return nil, parser.makeError(false)
		}
		varName := varNameToken.Value(IdentifierTP).(string)
		stms = append(stms, &StatementAst{
			StatementTP: VariableDeclareStatementTP,
			Statement: &VarDeclareAst{
				VarName: varName,
				VarType: varType,
			},
		})
		token, err := parser.getCurrentToken()
		if err != nil {
			return nil, err
		}
		if token.tp == CommaTP {
			parser.stepForward()
			continue
		}
		break
	}
	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return
}

func (parser *Parser) parseLetStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(LetTP, true)
	if !match {
		return nil, parser.makeError(false)
	}

	letVariable, err := parser.parseLetVariableAst()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(EqualTP, true)
	if !match {
		return nil, parser.makeError(false)
	}

	valueExpression, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, parser.makeError(false)
	}

	return &StatementAst{
		StatementTP: LetStatementTP,
		Statement: &LetStatementAst{
			LetVariable: letVariable,
			Value:       valueExpression,
		},
	}, nil
}

// In let statement: we don't allow let this =
func (parser *Parser) parseLetVariableAst() (*VariableAst, error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(true)
	}
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		return nil, parser.makeError(false)
	}
	arrayIndexExpression, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightSquareBracketTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return arrayIndexExpression, nil
}

func (parser *Parser) parseDoStatement() (stm *StatementAst, err error) {
	_, match := parser.expectToken(DoTp, true)
	if !match {
		return nil, parser.makeError(false)
	}
	funcCall, err := parser.parseFuncCall()
	if err != nil {
		return nil, err
	}

	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		return nil, parser.makeError(false)
	}
	// If no expressions.
	_, match = parser.expectToken(RightParentThesesTP, false)
	if match {
		parser.stepForward()
		return &CallAst{
			FuncProvider: funcProvider,
			FuncName:     funcName,
		}, nil
	}
	// Because we don't know the type of this function.
	// So we cannot check whether we need to put this as
	// the parameter of this method.
	expressions, err := parser.parseExpressions()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		err = parser.makeError(false)
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
		err = parser.makeError(false)
		return
	}
	funcProvider = funcName
	funcName = funcNameToken.content
	return
}

// if (condition) { do something } else { do something }
func (parser *Parser) parseIfStatement() (stm *StatementAst, err error) {
	match := parser.expectTokens(IfTP, LeftParentThesesTP)
	if !match {
		return nil, parser.makeError(false)
	}
	condition, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	match = parser.expectTokens(RightParentThesesTP, LeftBraceTP)
	if !match {
		return nil, parser.makeError(false)
	}

	ifTrueStatements, err := parser.parseIfTrueStatement()
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
			IfTrueStatements: ifTrueStatements,
			ElseStatements:   elseStatement,
		},
	}, nil
}

func (parser *Parser) parseIfTrueStatement() ([]*StatementAst, error) {
	ifTrueStatement, err := parser.parseStatements()
	if err != nil {
		return nil, err
	}
	_, match := parser.expectToken(RightBraceTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return ifTrueStatement, nil
}

func (parser *Parser) parseElseStatement() ([]*StatementAst, error) {
	_, match := parser.expectToken(ElseTP, false)
	if !match {
		return nil, nil
	}
	parser.stepForward()
	_, match = parser.expectToken(LeftBraceTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	statements, err := parser.parseStatements()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightBraceTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return statements, nil
}

func (parser *Parser) parseWhileStatement() (stm *StatementAst, err error) {
	match := parser.expectTokens(WhileTP, LeftParentThesesTP)
	if !match {
		return nil, parser.makeError(false)
	}
	condition, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	match = parser.expectTokens(RightParentThesesTP, LeftBraceTP)
	if !match {
		return nil, parser.makeError(false)
	}
	statements, err := parser.parseStatements()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightBraceTP, true)
	if !match {
		return nil, parser.makeError(false)
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
		return nil, parser.makeError(false)
	}

	// If no expressions.
	_, match = parser.expectToken(SemiColonTP, false)
	if match {
		parser.stepForward()
		return &StatementAst{
			StatementTP: ReturnStatementTP,
		}, nil
	}

	expressions, err := parser.parseExpressions()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(SemiColonTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return &StatementAst{
		StatementTP: ReturnStatementTP,
		Statement:   &ReturnStatementAst{Return: expressions},
	}, nil
}

func (parser *Parser) expectTokens(expectedTokenTPs ...TokenType) bool {
	for _, tokenType := range expectedTokenTPs {
		_, ok := parser.expectToken(tokenType, true)
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

func (parser *Parser) makeError(useCurrentPos bool) error {
	currentPos := parser.currentTokenPos
	if !useCurrentPos {
		currentPos--
	}
	if currentPos < 0 || currentPos >= len(parser.currentTokens) {
		return errors.New("unexpected token ends")
	}
	currentToken := parser.currentTokens[currentPos]
	return errors.New(fmt.Sprintf("syntax error near %s at line %d", currentToken.content,
		currentToken.line))
}

package internal

import (
	"strconv"
)

func buildExpressionsTree(ops []*OpAst, exprTerms []*ExpressionTerm) *ExpressionAst {
	if len(ops) == 0 {
		return &ExpressionAst{LeftExpr: exprTerms[0],}
	}
	if len(ops) == 1 {
		return &ExpressionAst{LeftExpr: exprTerms[0], Op: ops[0], RightExpr: exprTerms[1],}
	}
	expressionStack := make([]interface{}, 0, len(exprTerms))
	for _, exprTerm := range exprTerms {
		expressionStack = append(expressionStack, exprTerm)
	}
	for i := 2; len(expressionStack) > 2; i = i % len(expressionStack) {
		nextOp := ops[i-1]
		lastOp := ops[i-2]
		if lastOp.priority >= nextOp.priority {
			// We can merge last two expression to a new expression node.
			lastLeftExpression, lastRightExpression := expressionStack[i-2], expressionStack[i-1]
			newExpr := makeNewExpression(lastLeftExpression, lastRightExpression, lastOp)
			expressionStack = append(expressionStack[:i-1], expressionStack[i:]...)
			expressionStack[i-2] = newExpr
			ops = append(ops[:i-2], ops[i-1:]...)
			continue
		}
		i++
	}
	return makeNewExpression(expressionStack[0], expressionStack[1], ops[0])
}

func makeNewExpression(leftExpr interface{}, rightExpr interface{}, op *OpAst) *ExpressionAst {
	_, leftIsExpressionTerm := leftExpr.(*ExpressionTerm)
	_, rightIsExpressionTerm := rightExpr.(*ExpressionTerm)
	ret := new(ExpressionAst)
	if leftIsExpressionTerm {
		ret.LeftExpr = leftExpr.(*ExpressionTerm)
	} else {
		ret.LeftExpr = leftExpr.(*ExpressionAst)
	}
	if rightIsExpressionTerm {
		ret.RightExpr = rightExpr.(*ExpressionTerm)
	} else {
		ret.RightExpr = rightExpr.(*ExpressionAst)
	}
	ret.Op = op
	return ret
}

func (parser *Parser) parseExpressions() (exprs []*ExpressionAst, err error) {
	for parser.hasRemainTokens() {
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

func (parser *Parser) parseExpression() (ast *ExpressionAst, err error) {
	leftExprTerm, err := parser.parseExpressionTerm()
	if err != nil {
		return nil, err
	}
	var ops []*OpAst
	var exprTerms []*ExpressionTerm
	exprTerms = append(exprTerms, leftExprTerm)
	for parser.matchOp() {
		op, err := parser.parseOpAst()
		if err != nil {
			return nil, err
		}
		exprTerm, err := parser.parseExpressionTerm()
		if err != nil {
			return nil, err
		}
		ops = append(ops, op)
		exprTerms = append(exprTerms, exprTerm)
	}
	return buildExpressionsTree(ops, exprTerms), nil
}

func (parser *Parser) parseExpressionTerm() (expr *ExpressionTerm, err error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(false)
	}
	token, _ := parser.getCurrentToken()
	switch token.tp {
	case IntegerTP, CharacterTP, StringTP, TrueTP, FalseTP, NullTP, ThisTP:
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
		err = parser.makeError(true)
	}
	return
}

func (parser *Parser) parseConstantExpressionTerm() (term *ExpressionTerm, err error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(false)
	}
	term = new(ExpressionTerm)
	token := parser.currentTokens[parser.currentTokenPos]
	switch token.tp {
	case IntegerTP:
		term.Type = IntegerConstantTermType
		term.Value, _ = strconv.Atoi(token.content)
	case CharacterTP:
		term.Type = CharacterConstantTermType
		term.Value = token.content
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
		return nil, parser.makeError(true)
	}
	parser.stepForward()
	return
}

// Could be varName|varName[expression]|varName.funcName|varname[expression].funcName.
func (parser *Parser) parseSubRoutineCallExpressionOrVarExpressionTerm() (*ExpressionTerm, error) {
	expr, err := parser.parseVarExpressionTerm()
	if err != nil {
		return nil, err
	}
	if !parser.hasRemainTokens() {
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
		expr.Value, expr.Type = &ExpressionAst{
			LeftExpr: &ExpressionTerm{
				Type:  VarNameExpressionTermType,
				Value: expr.Value.(string),
			},
			Op:        &OpAst{OpTP: BinaryOPTP, Op: ArrayIndexOpTP, priority: 3},
			RightExpr: exprAst,
		}, ArrayIndexExpressionTermType
	case DotTP, LeftParentThesesTP:
		// Should be a funcCall
		parser.currentTokenPos--
		callAst, err := parser.parseFuncCall()
		if err != nil {
			return nil, err
		}
		expr.Value, expr.Type = callAst, SubRoutineCallTermType
	}
	return expr, nil
}

func (parser *Parser) parseVarExpressionTerm() (*ExpressionTerm, error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(true)
	}
	token, match := parser.expectToken(IdentifierTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	expr := new(ExpressionTerm)
	expr.Value, expr.Type = token.content, VarNameExpressionTermType
	return expr, nil
}

func (parser *Parser) parseSubExpressionTerm() (*ExpressionTerm, error) {
	_, match := parser.expectToken(LeftParentThesesTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	expr, err := parser.parseExpression()
	if err != nil {
		return nil, err
	}
	_, match = parser.expectToken(RightParentThesesTP, true)
	if !match {
		return nil, parser.makeError(false)
	}
	return &ExpressionTerm{
		Type:  SubExpressionTermType,
		Value: expr,
	}, nil
}

func (parser *Parser) parseNegationExpressionTerm() (*ExpressionTerm, error) {
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(false)
	}
	token, _ := parser.getCurrentToken()
	op := new(OpAst)
	op.OpTP = UnaryOPTP
	switch token.tp {
	case BooleanNegativeTP:
		op.Op = BooleanNegationOpTP
	case MinusTP:
		op.Op = NegationOpTP
	default:
		return nil, parser.makeError(true)
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
	if !parser.hasRemainTokens() {
		return nil, parser.makeError(false)
	}
	// Todo: we can use different priority for these.
	token := parser.currentTokens[parser.currentTokenPos]
	op := new(OpAst)
	op.OpTP = BinaryOPTP
	switch token.tp {
	case AddTP:
		op.Op = AddOpTP
		op.priority = 1
	case MinusTP:
		op.Op = MinusOpTP
		op.priority = 1
	case MultiplyTP:
		op.Op = MultipleOpTP
		op.priority = 1
	case DivideTP:
		op.Op = DivideOpTP
		op.priority = 1
	case AndTP:
		op.Op = AndOpTP
		op.priority = 1
	case OrTP:
		op.Op = OrOpTP
		op.priority = 1
	case GreaterTP:
		op.Op = GreaterOpTP
		op.priority = 1
	case LessTP:
		op.Op = LessOpTP
		op.priority = 1
	case EqualTP:
		op.Op = EqualOpTp
		op.priority = 1
	default:
		return nil, parser.makeError(true)
	}
	parser.stepForward()
	return op, nil
}

func (parser *Parser) matchOp() bool {
	if !parser.hasRemainTokens() {
		return false
	}
	token, _ := parser.getCurrentToken()
	op := new(OpAst)
	op.OpTP = BinaryOPTP
	switch token.tp {
	case AddTP, MinusTP, MultiplyTP, DivideTP, AndTP, OrTP, GreaterTP, LessTP, EqualTP:
		return true
	default:
		return false
	}
}

// Simple pretty tree printing.
//
//               80
//            ---  ---
//           /        \
//          48        92
//         -  -      -  -
//        /    \    /    \
//       35    52  10    15
//
//
//func printExpressionTree(expr *ExpressionAst, width int) {
//	// depth := getTreeDepth(expr)
//	indent := 1
//	printExpressionTreeNode(expr.LeftExpr, indent+2)
//	printExpressionTreeNode(expr.RightExpr, indent+2)
//}
//
//func printExpressionTreeNode(expr interface{}, depth int) {
//	// Todo
//}
//
//func getTreeDepth(expr *ExpressionAst) int {
//	leftTreeDepth := getTreeDepth0(expr.LeftExpr)
//	rightTreeDepth := getTreeDepth0(expr.RightExpr)
//	if leftTreeDepth >= rightTreeDepth {
//		return leftTreeDepth
//	}
//	return rightTreeDepth
//}
//
//func getTreeDepth0(expr interface{}) int {
//	if expr == nil {
//		return 0
//	}
//	_, ok := expr.(*ExpressionAst)
//	if ok {
//		return getTreeDepth2(expr.(*ExpressionAst))
//	}
//	return getTreeDepth1(expr.(*ExpressionTerm))
//}
//
//func getTreeDepth1(term *ExpressionTerm) int {
//	if term == nil {
//		return 0
//	}
//	switch term.Type {
//	case IntegerConstantTermType, CharacterConstantTermType, StringConstantTermType,
//		KeyWordConstantNullTermType, KeyWordConstantTrueTermType, KeyWordConstantThisTermType,
//		KeyWordConstantFalseTermType, VarNameExpressionTermType:
//		return 1
//	case ArrayIndexExpressionTermType:
//	case SubRoutineCallTermType:
//	case SubExpressionTermType:
//	case UnaryTermExpressionTermType:
//
//	}
//	return 0
//}
//
//func getTreeDepth2(expr *ExpressionAst) int {
//	if expr == nil {
//		return 0
//	}
//	return 0
//}

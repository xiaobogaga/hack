package compiler

//******************************************************//
//     existence checking                               //
//                          existence checking          //
//******************************************************//
func SymbolExistenceChecker(ast []*ClassAst) error {
	for _, classAst := range ast {
		err := classAst.existenceCheck()
		if err != nil {
			return err
		}
	}
	return nil
}

// Check whether symbol name exists in this class.
// For class variables: check whether the class exist.
// For funcLocalVariables and funcParams, If it's className, check whether the class exists.
// In statements and expression:
// * check whether the funcCall, param exists.
func (classAst *ClassAst) existenceCheck() error {
	err := classAst.checkClassVariablesExistence()
	if err != nil {
		return err
	}
	err = classAst.checkFuncVariablesExistence()
	if err != nil {
		return err
	}
	return nil
}

func (classAst *ClassAst) checkClassVariablesExistence() error {
	for _, ast := range classAst.classVariables {
		err := classAst.checkClassVariableExistence(ast)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkClassVariableExistence(ast *ClassVariableAst) error {
	// ast.symbolDesc = symbolTable.lookUpClassVar(classAst.className, ast.VariableName)
	if ast.VariableType.TP != ClassVariableType || symbolTable.isClassNameExist(ast.VariableType.Name) {
		return nil
	}
	return makeSemanticError("cannot find var %s's class %s", ast.VariableName, ast.VariableType.Name)
}

func (classAst *ClassAst) checkFuncVariablesExistence() error {
	for _, ast := range classAst.classFuncOrMethod {
		// ast.funcSymbol = symbolTable.lookUpFuncInClass(classAst.className, ast.FuncName)
		err := classAst.checkFuncVariableExistence(ast)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkFuncVariableExistence(ast *ClassFuncOrMethodAst) error {
	// Check func params
	for _, funcParam := range ast.Params {
		if funcParam.ParamTP.TP != ClassVariableType || symbolTable.isClassNameExist(funcParam.ParamTP.Name) {
			continue
		}
		return makeSemanticError("cannot find variable: %s's type %s at func %s.%s",
			funcParam.ParamName, funcParam.ParamTP.Name, classAst.className, ast.FuncName)
	}
	// Check returnType
	if ast.ReturnTP.TP == ClassVariableType && !symbolTable.isClassNameExist(ast.ReturnTP.Name) {
		return makeSemanticError("cannot find %s.%s's return type %s",
			classAst.className, ast.FuncName, ast.ReturnTP.Name)
	}
	// Check func Body.
	return classAst.checkFuncStatementsVariableExistence(ast)
}

func (classAst *ClassAst) checkFuncStatementsVariableExistence(ast *ClassFuncOrMethodAst) error {
	i := 0
	for ; i < len(ast.FuncBody); i++ {
		if ast.FuncBody[i].StatementTP != VariableDeclareStatementTP {
			break
		}
		err := classAst.checkVariableDeclareStatementVariableExistence(ast, ast.FuncBody[i])
		if err != nil {
			return err
		}
	}
	return classAst.checkStatementsVariableExistence(ast, ast.FuncBody[i:])
}

func (classAst *ClassAst) checkVariableDeclareStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	variableDeclareAst := statement.Statement.(*VarDeclareAst)
	if variableDeclareAst.VarType.TP == ClassVariableType && !symbolTable.isClassNameExist(variableDeclareAst.VarType.Name) {
		return makeSemanticError("cannot find variable %+v 's type %s at %s.%s", variableDeclareAst.VarName,
			variableDeclareAst.VarType.Name, classAst.className, ast.FuncName)
	}
	return nil
}

func (classAst *ClassAst) checkStatementsVariableExistence(ast *ClassFuncOrMethodAst, statements []*StatementAst) error {
	for _, statement := range statements {
		err := classAst.checkStatementVariableExistence(ast, statement)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	switch statement.StatementTP {
	case LetStatementTP:
		return classAst.checkLetStatementVariableExistence(ast, statement)
	case IfStatementTP:
		return classAst.checkIfStatementVariableExistence(ast, statement)
	case WhileStatementTP:
		return classAst.checkWhileStatementVariableExistence(ast, statement)
	case DoStatementTP:
		return classAst.checkDoStatementVariableExistence(ast, statement)
	case ReturnStatementTP:
		return classAst.checkReturnStatementVariableExistence(ast, statement)
	}
	return nil
}

func (classAst *ClassAst) checkLetStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	letStatement := (statement).Statement.(*LetStatementAst)
	// Because variable declare statement appears before other statements.
	// So we can simply check variable existence by using symbolTable.
	varSymbol, err := symbolTable.lookUpVarInFunc(classAst.className, ast.FuncName, letStatement.LetVariable.VarName)
	if err != nil {
		return err
	}
	if varSymbol == nil {
		return makeSemanticError("cannot find such variable %s at %s.%s", letStatement.LetVariable.VarName, classAst.className, ast.FuncName)
	}
	letStatement.LetVariable.symbolDesc = varSymbol
	err = classAst.checkVariableExistenceInExpression(ast, letStatement.LetVariable.ArrayIndex)
	if err != nil {
		return err
	}
	return classAst.checkVariableExistenceInExpression(ast, letStatement.Value)
}

func (classAst *ClassAst) checkIfStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	ifStatement := statement.Statement.(*IfStatementAst)
	err := classAst.checkVariableExistenceInExpression(ast, ifStatement.Condition)
	if err != nil {
		return err
	}
	err = classAst.checkStatementsVariableExistence(ast, ifStatement.IfTrueStatements)
	if err != nil {
		return err
	}
	return classAst.checkStatementsVariableExistence(ast, ifStatement.ElseStatements)
}

func (classAst *ClassAst) checkWhileStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	whileStatement := statement.Statement.(*WhileStatementAst)
	err := classAst.checkVariableExistenceInExpression(ast, whileStatement.Condition)
	if err != nil {
		return err
	}
	return classAst.checkStatementsVariableExistence(ast, whileStatement.Statements)
}

func (classAst *ClassAst) checkDoStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	doStatement := statement.Statement.(*DoStatementAst)
	return classAst.checkFuncCallVariableExistence(ast, doStatement.Call)
}

func (classAst *ClassAst) checkFuncCallVariableExistence(ast *ClassFuncOrMethodAst, call *CallAst) error {
	fn, err := symbolTable.lookUpFuncInSpecificClassAndFunc(classAst.className, ast.FuncName, call)
	if err != nil {
		return err
	}
	if fn == nil {
		if call.FuncProvider != "" {
			return makeSemanticError("cannot find func %s.%s at class %s.%s", call.FuncProvider,
				call.FuncName, classAst.className, ast.FuncName)
		}
		return makeSemanticError("cannot find func %s at class %s.%s", call.FuncName, classAst.className, ast.FuncName)
	}
	if len(fn.FuncParamsSymbolDesc) != len(call.Params) {
		return makeSemanticError("func call %s.%s doesn't match parameter size", call.FuncProvider, call.FuncName)
	}
	call.FuncSymbolTable = fn
	return classAst.checkVariableExistenceInExpressions(ast, call.Params)
}

func (classAst *ClassAst) checkReturnStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	if statement.Statement == nil {
		return nil
	}
	retStatement := statement.Statement.(*ReturnStatementAst)
	for _, expr := range retStatement.Return {
		err := classAst.checkVariableExistenceInExpression(ast, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkVariableExistenceInExpression(ast *ClassFuncOrMethodAst, expr *ExpressionAst) error {
	if expr == nil {
		return nil
	}
	var err error
	if expr.LeftExpr != nil {
		_, isLeftExprExpressionTerm := expr.LeftExpr.(*ExpressionTerm)
		if isLeftExprExpressionTerm {
			err = classAst.checkVariableExistenceInExpressionTerm(ast, expr.LeftExpr.(*ExpressionTerm))
		} else {
			err = classAst.checkVariableExistenceInExpression(ast, expr.LeftExpr.(*ExpressionAst))
		}
	}
	if err != nil {
		return err
	}
	if expr.RightExpr != nil {
		_, isRightExprExpressionTerm := expr.RightExpr.(*ExpressionTerm)
		if isRightExprExpressionTerm {
			err = classAst.checkVariableExistenceInExpressionTerm(ast, expr.RightExpr.(*ExpressionTerm))
		} else {
			err = classAst.checkVariableExistenceInExpression(ast, expr.RightExpr.(*ExpressionAst))
		}
	}
	return err
}

func (classAst *ClassAst) checkVariableExistenceInExpressionTerm(ast *ClassFuncOrMethodAst, exprTerm *ExpressionTerm) error {
	if exprTerm == nil {
		return nil
	}
	switch exprTerm.Type {
	case IntegerConstantTermType, StringConstantTermType, KeyWordConstantFalseTermType, KeyWordConstantTrueTermType, KeyWordConstantNullTermType, CharacterConstantTermType:
		return nil
	case KeyWordConstantThisTermType:
		// If this method is unction, return err.
		if ast.FuncTP != ClassFuncType {
			return nil
		}
		return makeSemanticError("cannot use this in static function %s.%s", classAst.className, ast.FuncName)
	case VarNameExpressionTermType:
		symbol, err := symbolTable.lookUpVarInFunc(classAst.className, ast.FuncName, exprTerm.Value.(string))
		if err != nil {
			return err
		}
		if symbol == nil {
			return makeSemanticError("cannot find variable %s in %s.%s", exprTerm.Value.(string), classAst.className, ast.FuncName)
		}
	case ArrayIndexExpressionTermType:
		varName := exprTerm.Value.(*ExpressionAst).LeftExpr.(*ExpressionTerm).Value.(string)
		varSymbol, err := symbolTable.lookUpVarInFunc(classAst.className, ast.FuncName, varName)
		if err != nil {
			return err
		}
		if varSymbol == nil {
			return makeSemanticError("cannot find variable %s in %s.%s", exprTerm.Value.(*ExpressionAst).LeftExpr.(*ExpressionTerm).Value.(string), classAst.className, ast.FuncName)
		}
		return classAst.checkVariableExistenceInExpression(ast, exprTerm.Value.(*ExpressionAst).RightExpr.(*ExpressionAst))
	case SubRoutineCallTermType:
		return classAst.checkFuncCallVariableExistence(ast, exprTerm.Value.(*CallAst))
	case SubExpressionTermType:
		return classAst.checkVariableExistenceInExpression(ast, exprTerm.Value.(*ExpressionAst))
	case UnaryTermExpressionTermType:
		return classAst.checkVariableExistenceInExpressionTerm(ast, exprTerm.Value.(*ExpressionTerm))
	}
	return nil
}

func (classAst *ClassAst) checkVariableExistenceInExpressions(ast *ClassFuncOrMethodAst, exprs []*ExpressionAst) error {
	for _, expr := range exprs {
		err := classAst.checkVariableExistenceInExpression(ast, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkVarExpressionExistence(ast *ClassFuncOrMethodAst, variable *VariableAst) error {
	varSymbol, err := symbolTable.lookUpVarInFunc(classAst.className, ast.FuncName, variable.VarName)
	if err != nil {
		return err
	}
	if varSymbol == nil {
		return makeSemanticError("cannot find such variable %s at %s.%s", variable.VarName, classAst.className, ast.FuncName)
	}
	variable.symbolDesc = varSymbol
	return classAst.checkVariableExistenceInExpression(ast, variable.ArrayIndex)
}

//********************************************************************************//
//                     type                      type                             //
//                               checking                        checking         //
//********************************************************************************//
func typeChecker(ast []*ClassAst) error {
	for _, classAst := range ast {
		err := classAst.typeChecker0()
		if err != nil {
			return err
		}
	}
	return nil
}

// For type checker. we only check statements.
// There are five different statements:
// * Let statement
// * If statement
func (classAst *ClassAst) typeChecker0() error {
	for _, ast := range classAst.classFuncOrMethod {
		err := classAst.typeCheckMethod(ast)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) typeCheckMethod(methodAst *ClassFuncOrMethodAst) (err error) {
	return classAst.typeCheckStatements(methodAst, methodAst.FuncBody)
}

func (classAst *ClassAst) typeCheckStatements(methodAst *ClassFuncOrMethodAst, statements []*StatementAst) (err error) {
	for _, stm := range statements {
		switch stm.StatementTP {
		case VariableDeclareStatementTP:
			continue
		case LetStatementTP:
			err = classAst.typeCheckLetStatement(methodAst, stm.Statement.(*LetStatementAst))
		case IfStatementTP:
			err = classAst.typeCheckIfStatement(methodAst, stm.Statement.(*IfStatementAst))
		case WhileStatementTP:
			err = classAst.typeCheckWhileStatement(methodAst, stm.Statement.(*WhileStatementAst))
		case DoStatementTP:
			err = classAst.typeCheckDoStatement(methodAst, stm.Statement.(*DoStatementAst))
		case ReturnStatementTP:
			err = classAst.typeCheckReturnStatement(methodAst, stm)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) typeCheckLetStatement(methodAst *ClassFuncOrMethodAst, ast *LetStatementAst) error {
	l := &ast.LetVariable.symbolDesc.variableType
	r, err := classAst.getAndCheckExpressionType(methodAst, ast.Value)
	if err != nil {
		return err
	}
	if classAst.checkMatch1(l, r) {
		return nil
	}
	return makeSemanticError("expected %+v but %+v at %s.%s", l, r, classAst.className, methodAst.FuncName)
}

func (classAst *ClassAst) typeCheckIfStatement(methodAst *ClassFuncOrMethodAst, ast *IfStatementAst) error {
	t, err := classAst.getAndCheckExpressionType(methodAst, ast.Condition)
	if err != nil {
		return err
	}
	if !classAst.checkMatch1(&VariableType{TP: BooleanVariableType,}, t) {
		return makeSemanticError("expected %s but %+v at %s.%s", "bool", t, classAst.className, methodAst.FuncName)
	}
	err = classAst.typeCheckStatements(methodAst, ast.IfTrueStatements)
	if err != nil {
		return err
	}
	return classAst.typeCheckStatements(methodAst, ast.ElseStatements)
}

func (classAst *ClassAst) typeCheckWhileStatement(methodAst *ClassFuncOrMethodAst, ast *WhileStatementAst) error {
	t, err := classAst.getAndCheckExpressionType(methodAst, ast.Condition)
	if err != nil {
		return err
	}
	if !classAst.checkMatch1(&VariableType{TP: BooleanVariableType,}, t) {
		return makeSemanticError("expected %s but %+v at %s.%s", "bool", t, classAst.className, methodAst.FuncName)
	}
	return classAst.typeCheckStatements(methodAst, ast.Statements)
}

func (classAst *ClassAst) typeCheckDoStatement(methodAst *ClassFuncOrMethodAst, ast *DoStatementAst) error {
	funcSymbolDesc := ast.Call.FuncSymbolTable
	for i, expr := range ast.Call.Params {
		realType, err := classAst.getAndCheckExpressionType(methodAst, expr)
		if err != nil {
			return err
		}
		expectType := funcSymbolDesc.FuncParamsSymbolDesc[i].variableType
		if !classAst.checkMatch1(&expectType, realType) {
			return makeSemanticError("expected %+v but %+v at %s.%s %d-th param at %s.%s", expectType,
				realType, ast.Call.FuncProvider, ast.Call.FuncName, i, classAst.className, methodAst.FuncName)
		}
	}
	return nil
}

func (classAst *ClassAst) typeCheckReturnStatement(methodAst *ClassFuncOrMethodAst, ast *StatementAst) (err error) {
	var returnTP *VariableType
	if ast.Statement != nil {
		returnTP, err = classAst.getAndCheckExpressionType(methodAst, ast.Statement.(*ReturnStatementAst).Return[0])
	}
	if err != nil {
		return err
	}
	if !classAst.checkMatch1(&methodAst.ReturnTP, returnTP) {
		return makeSemanticError("func %s.%s return type doesn't match, expected: %+v but: %+v",
			classAst.className, methodAst.FuncName, methodAst.ReturnTP, returnTP)
	}
	return nil
}

// typeCheckExpression checks whether the types match in this expression and return the result type of this expression.
func (classAst *ClassAst) getAndCheckExpressionType(methodAst *ClassFuncOrMethodAst, expr *ExpressionAst) (*VariableType, error) {
	// We have bool, char, int and we have multiple kinds of operations: + - * / >= <= == && ||
	leftT, err := classAst.getAndCheckExpressionType0(methodAst, expr.LeftExpr)
	if err != nil {
		return nil, err
	}
	if expr.RightExpr == nil {
		return leftT, nil
	}
	rightT, err := classAst.getAndCheckExpressionType0(methodAst, expr.RightExpr)
	if err != nil {
		return nil, err
	}
	tp, err := classAst.checkMatch0(methodAst, leftT, rightT, expr.Op)
	expr.TP = tp
	return tp, err
}

func (classAst *ClassAst) getAndCheckExpressionType0(methodAst *ClassFuncOrMethodAst, expr interface{}) (t *VariableType, err error) {
	_, ok := expr.(*ExpressionAst)
	if ok {
		return classAst.getAndCheckExpressionType(methodAst, expr.(*ExpressionAst))
	}
	exprTerm := expr.(*ExpressionTerm)
	switch exprTerm.Type {
	case IntegerConstantTermType:
		exprTerm.TP = &VariableType{TP: IntVariableType}
	case CharacterConstantTermType:
		exprTerm.TP = &VariableType{TP: CharVariableType}
	case StringConstantTermType:
		exprTerm.TP = &VariableType{TP: ClassVariableType, Name: "String"}
	case KeyWordConstantTrueTermType, KeyWordConstantFalseTermType:
		exprTerm.TP = &VariableType{TP: BooleanVariableType}
	case KeyWordConstantThisTermType:
		exprTerm.TP = &VariableType{TP: ClassVariableType, Name: "this"}
	case KeyWordConstantNullTermType:
		exprTerm.TP = &VariableType{TP: ClassVariableType, Name: "null"}
	case VarNameExpressionTermType:
		exprTerm.TP = classAst.getVariableType(methodAst, exprTerm.Value.(string))
	case UnaryTermExpressionTermType:
		t, err = classAst.getAndCheckExpressionType0(methodAst, exprTerm.Value)
		if err != nil {
			return nil, err
		}
		exprTerm.TP = t
	case SubExpressionTermType:
		t, err = classAst.getAndCheckExpressionType(methodAst, exprTerm.Value.(*ExpressionAst))
		if err != nil {
			return nil, err
		}
		exprTerm.TP = t
	case ArrayIndexExpressionTermType:
		t, err = classAst.getAndCheckExpressionType(methodAst, exprTerm.Value.(*ExpressionAst))
		if err != nil {
			return nil, err
		}
		exprTerm.TP = t
	case SubRoutineCallTermType:
		exprTerm.TP = &exprTerm.Value.(*CallAst).FuncSymbolTable.FuncReturnSymbolDesc.variableType
	}
	err = classAst.checkTypeMatchOnUnaryOp(methodAst, t, exprTerm.UnaryOp)
	if err != nil {
		return nil, err
	}
	return exprTerm.TP, nil
}

func (classAst *ClassAst) getVariableType(methodAst *ClassFuncOrMethodAst, varName string) *VariableType {
	// Find variable type on class variable first.
	varSymbol, _ := symbolTable.lookUpVarInFunc(classAst.className, methodAst.FuncName, varName)
	return &varSymbol.variableType
}

func (classAst *ClassAst) checkMatch0(methodAst *ClassFuncOrMethodAst, l, r *VariableType, op *OpAst) (*VariableType, error) {
	if op.Op == AndOpTP || op.Op == OrOpTP {
		return l, classAst.checkBooleanMatch(methodAst, l, r)
	}
	if op.Op == EqualOpTp {
		return &VariableType{TP: BooleanVariableType}, classAst.checkEqualOpMatch(methodAst, l, r)
	}
	if op.Op == ArrayIndexOpTP {
		if l.TP != ClassVariableType {
			return nil, makeSemanticError("type %+v doesn't support index at %s.%s", l, classAst.className, methodAst.FuncName)
		}
		if isIntegerCompatibleType(r) {
			return nil, makeSemanticError("type %+v doesn't support array index at %s.%s", r, classAst.className, methodAst.FuncName)
		}
		// Todo: what type should I return here.
		return &VariableType{TP: IntVariableType,}, nil
	}
	// Otherwise l and r should be both a integer compatible type
	if !isIntegerCompatibleType(l) || !isIntegerCompatibleType(r) {
		return nil, makeSemanticError("unmatched type: l: %+v, r: %+v, op: %+v at %s.%s", l, r, op, classAst.className, methodAst.FuncName)
	}
	if isComparatorOpType(op) {
		return &VariableType{TP: BooleanVariableType}, nil
	}
	// We return the ClassVariableType
	if l.TP == ClassVariableType {
		return l, nil
	}
	if r.TP == ClassVariableType {
		return r, nil
	}
	// Then we consider return IntVariableType
	if l.TP == IntVariableType {
		return l, nil
	}
	if r.TP == IntVariableType {
		return r, nil
	}
	return l, nil
}

func isIntegerCompatibleType(l *VariableType) bool {
	return l.TP == IntVariableType || l.TP == CharVariableType || l.TP == ClassVariableType
}

func isComparatorOpType(op *OpAst) bool {
	return op.Op == GreaterOpTP || op.Op == LessOpTP || op.Op == EqualOpTp
}

func (classAst *ClassAst) checkBooleanMatch(methodAst *ClassFuncOrMethodAst, l, r *VariableType) error {
	if l.TP == BooleanVariableType && r.TP == BooleanVariableType {
		return nil
	}
	return makeSemanticError("unmatched boolean type l: %+v, r: %+v", l, r)
}

func (classAst *ClassAst) checkEqualOpMatch(methodAst *ClassFuncOrMethodAst, l, r *VariableType) error {
	if isIntegerCompatibleType(l) && isIntegerCompatibleType(r) {
		// If they are both class types. They must be the same class type.
		if l.TP == ClassVariableType && r.TP == ClassVariableType && l.Name != r.Name && (l.Name != "null" && r.Name != "null") {
			return makeSemanticError("unmatched type l: %s, r: %s, op: = at %s.%s", l, r, classAst.className, methodAst.FuncName)
		}
		return nil
	}
	if l.TP != r.TP {
		return makeSemanticError("unmatched type l: %s, r: %s, op: = at %s.%s", l, r, classAst.className, methodAst.FuncName)
	}
	return nil
}

func (classAst *ClassAst) checkMatch1(expected, real *VariableType) bool {
	if expected.TP == VoidVariableType {
		return real == nil || real.TP == VoidVariableType
	}
	if real == nil {
		return false
	}
	switch expected.TP {
	case BooleanVariableType:
		return real.TP == BooleanVariableType
	case CharVariableType:
		return real.TP == CharVariableType || real.TP == IntVariableType
	case IntVariableType:
		return real.TP == IntVariableType || real.TP == CharVariableType
	case ClassVariableType:
		if expected.Name == "Array" || real.Name == "Array" {
			return classAst.checkArrayTypeMatch(expected, real)
		}
		if real.TP == IntVariableType || (real.TP == ClassVariableType && (real.Name == expected.Name || real.Name == "null")) {
			return true
		}
		if real.TP == ClassVariableType && real.Name == expected.Name {
			return true
		}
		// For Object array and array can be transformed to each other.
	}
	return false
}

func (classAst *ClassAst) checkArrayTypeMatch(expected *VariableType, real *VariableType) bool {
	// object can be transformed to array.
	if expected.Name == "Array" {
		return real.TP == ClassVariableType
	}
	return real.Name == "Array"
}

func (classAst *ClassAst) checkTypeMatchOnUnaryOp(methodAst *ClassFuncOrMethodAst, t *VariableType, op *OpAst) error {
	if op == nil {
		return nil
	}
	switch op.Op {
	case NegationOpTP:
		if t.TP != IntVariableType && t.TP != CharVariableType {
			return makeSemanticError("type %+v doesn't support - at %s.%s", t, classAst.className, methodAst.FuncName)
		}
	case BooleanNegationOpTP:
		if t.TP != BooleanVariableType {
			return makeSemanticError("type %+v doesn't support ~ at %s.%s", t, classAst.className, methodAst.FuncName)
		}
	}
	return nil
}

//********************************************************************************//
//                     return                      return                         //
//                               analysis                        analysis         //
//********************************************************************************//
func methodReturnAnalysisOnClasses(asts []*ClassAst) error {
	for _, ast := range asts {
		err := ast.methodReturnAnalysis()
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) methodReturnAnalysis() error {
	for _, method := range classAst.classFuncOrMethod {
		err := classAst.methodReturnAnalysis0(method)
		if err != nil {
			return err
		}
	}
	return nil
}

// Check those func who have return a non-void return type, should as least have a return.
func (classAst *ClassAst) methodReturnAnalysis0(method *ClassFuncOrMethodAst) error {
	hasReturn, err := classAst.statementReturnAnalysis(method, method.FuncBody)
	if err != nil {
		return err
	}
	if !hasReturn {
		return makeSemanticError("method %s.%s doesn't return", classAst.className, method.FuncName)
	}
	return nil
}

func (classAst *ClassAst) ifElseReturnAnalysis(method *ClassFuncOrMethodAst, ifAst *IfStatementAst) bool {
	if len(ifAst.IfTrueStatements) <= 0 || len(ifAst.ElseStatements) <= 0 {
		return false
	}
	l, _ := classAst.statementReturnAnalysis(method, ifAst.IfTrueStatements)
	r, _ := classAst.statementReturnAnalysis(method, ifAst.ElseStatements)
	return l && r
}

// statementReturnAnalysis check whether those statements have a valid return semantics: at most one valid return
// on a valid path.
// How: Observing that what we only need to verify if-else clause. So we can abstract the program as following:
// func fName() {
//    if condition {
//    }
//    return // must can return
//    do print() // unreachable code.
// }
// * If we meet a return statement, then we say this function can return.
// * If doesn't have any return statement, we check whether there are full-returned-if-else.
// what's called full-returned-if-else, this is an example:
// func fName() {
//	if {
//    return
//  }
// }
// Or
// func fName() {
//	if {
//    return
//  } else {
//    return
//  }
// }
//
func (classAst *ClassAst) statementReturnAnalysis(method *ClassFuncOrMethodAst, statements []*StatementAst) (bool, error) {
	i, hasReturn := 0, false
	for ; i < len(statements); i++ {
		stm := statements[i]
		switch stm.StatementTP {
		case VariableDeclareStatementTP, LetStatementTP, WhileStatementTP, DoStatementTP:
			continue
		case ReturnStatementTP:
			hasReturn = true
			goto Exit
		case IfStatementTP:
			ifElseBothHasReturn := classAst.ifElseReturnAnalysis(method, stm.Statement.(*IfStatementAst))
			if ifElseBothHasReturn {
				hasReturn = true
				goto Exit
			}
		}
	}
Exit:
	i++
	if !hasReturn {
		return false, nil
	}
	if hasReturn && i < len(method.FuncBody) {
		return true, makeSemanticError("unreachable code at %s.%s", classAst.className, method.FuncName)
	}
	return true, nil
}

package compiler

func SymbolExistenceChecker(ast []*ClassAst) error {
	// Todo
	for _, classAst := range ast {
		err := symbolExistenceCheckerForSingleClassAst(classAst)
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
func symbolExistenceCheckerForSingleClassAst(classAst *ClassAst) error {
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
	if ast.VariableType.TP == ClassVariableType || isClassNameExist(ast.VariableType.Name) {
		return nil
	}
	return makeSemanticError("class %s cannot find", ast.VariableType.Name)
}

func (classAst *ClassAst) checkFuncVariablesExistence() error {
	for _, ast := range classAst.classFuncOrMethod {
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
		if funcParam.ParamTP.TP != ClassVariableType || isClassNameExist(funcParam.ParamTP.Name) {
			continue
		}
		return makeSemanticError("cannot find %s class name at func %s as it's params",
			funcParam.ParamTP.Name, ast.FuncName)
	}
	// Check returnType
	if ast.ReturnTP.TP == ClassReturnType && !isClassNameExist(ast.ReturnTP.Name) {
		return makeSemanticError("cannot find %s class name at func %s as it's return type",
			ast.ReturnTP.Name, ast.FuncName)
	}
	// Check func Body.
	return classAst.checkFunStatementsVariableExistence(ast)
}

func (classAst *ClassAst) checkFunStatementsVariableExistence(ast *ClassFuncOrMethodAst) error {
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
	if variableDeclareAst.VarType.TP == ClassVariableType && !isClassNameExist(variableDeclareAst.VarType.Name) {
		return makeSemanticError("variable %s type class %s doesn't exist at func %s", variableDeclareAst.VarNames,
			variableDeclareAst.VarType.Name, ast.FuncName)
	}
	return nil
}

func (classAst *ClassAst) checkLetStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	letStatement := (statement).Statement.(*LetStatementAst)
	// Because variable declare statement appears before other statements.
	// So we can simply check variable existence by using symbolTable.
	if !classAst.isNameExist(letStatement.LetVariable.VarName, ast.FuncName) {
		return makeSemanticError("undeclared variable: %s at func: %s", letStatement.LetVariable.VarName,
			ast.FuncName)
	}
	err := classAst.checkVariableExistenceInExpression(ast, letStatement.LetVariable.ArrayIndex)
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
	doStatement := statement.Statement.(DoStatementAst)
	return classAst.checkFuncCallVariableExistence(ast, doStatement.Call)
}

func (classAst *ClassAst) checkFuncCallVariableExistence(ast *ClassFuncOrMethodAst, call *CallAst) error {
	fn := classAst.getFuncDefRef(ast, call.FuncName, call.FuncProvider)
	if fn == nil {
		if call.FuncProvider != "" {
			return makeSemanticError("cannot find such name %s.%s", call.FuncProvider,
				call.FuncName)
		}
		return makeSemanticError("cannot find such name: %s", call.FuncName)
	}
	// Todo check parameter length match:
	if len(fn.FuncParamsSymbolDesc) != len(ast.Params) {
		return makeSemanticError("func %s doesn't match parameter size", len(fn.FuncParamsSymbolDesc))
	}
	// Todo: check whether we need to set fn to call.
	return classAst.checkVariableExistenceInExpressions(ast, call.Params)
}

func (classAst *ClassAst) checkReturnStatementVariableExistence(ast *ClassFuncOrMethodAst, statement *StatementAst) error {
	retStatement := statement.Statement.(*ReturnStatementAst)
	for _, expr := range retStatement.Return {
		err := classAst.checkVariableExistenceInExpression(ast, expr)
		if err != nil {
			return err
		}
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

func (classAst *ClassAst) checkVariableExistenceInFunc(ast *ClassFuncOrMethodAst, call *CallAst) error {
	for _, expr := range call.Params {
		err := classAst.checkVariableExistenceInExpression(ast, expr)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) checkVariableExistenceInExpression(ast *ClassFuncOrMethodAst, expr *ExpressionAst) error {
	_, isLeftExprExpressionTerm := expr.LeftExpr.(*ExpressionTerm)
	var err error
	if isLeftExprExpressionTerm {
		err = classAst.checkVariableExistenceInExpressionTerm(ast, expr.LeftExpr.(*ExpressionTerm))
	} else {
		err = classAst.checkVariableExistenceInExpression(ast, expr.LeftExpr.(*ExpressionAst))
	}
	if err != nil {
		return err
	}
	_, isRightExprExpressionTerm := expr.RightExpr.(*ExpressionTerm)
	if isRightExprExpressionTerm {
		err = classAst.checkVariableExistenceInExpressionTerm(ast, expr.RightExpr.(*ExpressionTerm))
	} else {
		err = classAst.checkVariableExistenceInExpression(ast, expr.RightExpr.(*ExpressionAst))
	}
	return err
}

func (classAst *ClassAst) checkVariableExistenceInExpressionTerm(ast *ClassFuncOrMethodAst, exprTerm *ExpressionTerm) error {
	switch exprTerm.Type {
	case IntegerConstantTermType, StringConstantTermType, KeyWordConstantFalseTermType, KeyWordConstantTrueTermType, KeyWordConstantNullTermType:
		return nil
	case KeyWordConstantThisTermType:
		// If this method is unction, return err.
		if ast.FuncTP != ClassFuncType {
			return nil
		}
		return makeSemanticError("cannot use this in functions")
	case VarNameExpressionTermType:
		return classAst.checkVarExpressionExistence(ast, exprTerm.Value.(*VariableAst))
	case ArrayIndexExpressionTermType:
		varName := exprTerm.Value.(*ExpressionAst).LeftExpr.(*ExpressionTerm).Value.(string)
		if !classAst.isNameExist(varName, ast.FuncName) {
			return makeSemanticError("cannot find variable %s in func %s", varName, ast.FuncName)
		}
		return classAst.checkVariableExistenceInExpression(ast, exprTerm.Value.(*ExpressionAst).RightExpr.(*ExpressionAst))
	case SubRoutineCallTermType:
		callAst := exprTerm.Value.(*CallAst)
		fn := classAst.getFuncDefRef(ast, callAst.FuncName, callAst.FuncProvider)
		if fn == nil {
			return makeSemanticError("cannot find such func: %s.%s", callAst.FuncProvider, callAst.FuncName)
		}
		if len(fn.FuncParamsSymbolDesc) != len(callAst.Params) {
			return makeSemanticError("func %s.%s call params doesn't match", callAst.FuncProvider, callAst.FuncName)
		}
		return classAst.checkVariableExistenceInExpressions(ast, callAst.Params)
	case SubExpressionTermType:
		return classAst.checkVariableExistenceInExpression(ast, exprTerm.Value.(*ExpressionAst))
	case UnaryTermExpressionTermType:
		return classAst.checkVariableExistenceInExpression(ast, exprTerm.Value.(*ExpressionAst))
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
	classSymbols := symbolTable[classAst.className]
	if classSymbols.VariablesSymbolTable[variable.VarName] == nil &&
		classSymbols.FuncSymbolTable[ast.FuncName].FuncParamsSymbolDescMap[variable.VarName] == nil &&
		classSymbols.FuncSymbolTable[ast.FuncName].FuncLocalVariableDesc[variable.VarName] == nil {
		return makeSemanticError("cannot find %s variable in %s func", variable.VarName, ast.FuncName)
	}
	return classAst.checkVariableExistenceInExpression(ast, variable.ArrayIndex)
}

func isClassNameExist(className string) bool {
	_, ok := symbolTable[className]
	return ok
}

func (classAst *ClassAst) isNameExist(name string, funcName string) bool {
	funcSymbols, ok := symbolTable[classAst.className].FuncSymbolTable[funcName]
	if !ok {
		return false
	}
	return funcSymbols.FuncParamsSymbolDescMap[name] == nil ||
		funcSymbols.FuncLocalVariableDesc[name] == nil
}

// In java, there are many restrictions in static or constructor method. For example,
// In static method, cannot use this, class non-static variables.
// In jack language:
func (classAst *ClassAst) getFuncDefRef(methodAst *ClassFuncOrMethodAst, funcName string, funcProvider string) *SymbolsInFunc {
	// Cannot use this in
	if (methodAst.FuncTP == ClassConstructorType || methodAst.FuncTP == ClassFuncType) && (funcProvider == "" || funcProvider == "this") {
		println("cannot use this or method call in constructor or func")
		return nil
	}
	if funcProvider == "" || funcProvider == "this" {
		// Try search function in current class.
		return symbolTable.lookUpFuncInClass(classAst.className, funcName)
	}
	// funcProvider maybe a variable name.
	ret, foundSuchVariable := classAst.getFuncDefRefByVariableOrClassName(methodAst, funcName, funcProvider)
	if foundSuchVariable {
		return ret
	}
	// If not, it should be a className
	_, ok := symbolTable[funcProvider]
	if !ok {
		return nil
	}
	return symbolTable[funcProvider].FuncSymbolTable[funcName]
}

func (classAst *ClassAst) getFuncDefRefByVariableOrClassName(methodAst *ClassFuncOrMethodAst, funcName string,
	varNameOrClassName string) (symbols *SymbolsInFunc, foundVariableButNotSuchFunc bool) {
	ret := symbolTable.lookUpVarInFunc(classAst.className, methodAst.FuncName, varNameOrClassName)
	if ret == nil {
		return classAst.getFuncDefRefFromByClassName(methodAst, funcName, varNameOrClassName)
	}
	if ret.variableType.TP != ClassVariableType || ret.name == "null" {
		return nil, true
	}
	funcs, ok := symbolTable[ret.variableType.Name]
	if !ok {
		return nil, true
	}
	return funcs.FuncSymbolTable[funcName], true
}

func (classAst *ClassAst) getFuncDefRefFromByClassName(methodAst *ClassFuncOrMethodAst, funcName string,
	className string) (symbols *SymbolsInFunc, foundVariableButNotSuchFunc bool) {
	classSymbol := symbolTable.lookUpClass(className)
	if classSymbol == nil {
		return nil, false
	}
	funcSymbol, ok := classSymbol.FuncSymbolTable[funcName]
	if !ok {
		return nil, true
	}
	return funcSymbol, false
}

func typeChecker(ast []*ClassAst) error {
	for _, classAst := range ast {
		err := classAst.typeCheckerSingleClassAst()
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
func (classAst *ClassAst) typeCheckerSingleClassAst() error {
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
			err = classAst.typeCheckReturnStatement(methodAst, stm.Statement.(*ReturnStatementAst))
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) typeCheckLetStatement(methodAst *ClassFuncOrMethodAst, ast *LetStatementAst) error {
	l, err := classAst.getVariableAstType(methodAst, ast.LetVariable)
	if err != nil {
		return err
	}
	r, err := classAst.getAndCheckExpressionType(methodAst, ast.Value)
	if err != nil {
		return err
	}
	if classAst.checkMatch1(l, r) {
		return nil
	}
	return makeSemanticError("expected %+v but real %+v", l, r)
}

func (classAst *ClassAst) typeCheckIfStatement(methodAst *ClassFuncOrMethodAst, ast *IfStatementAst) error {
	t, err := classAst.getAndCheckExpressionType(methodAst, ast.Condition)
	if err != nil {
		return err
	}
	if !classAst.checkMatch1(&VariableType{TP: BooleanVariableType,}, t) {
		return makeSemanticError("expected %+v but real %+v", "bool", t)
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
		return makeSemanticError("expected %+v but real %+v", "bool", t)
	}
	return classAst.typeCheckStatements(methodAst, ast.Statements)
}

func (classAst *ClassAst) typeCheckDoStatement(methodAst *ClassFuncOrMethodAst, ast *DoStatementAst) error {
	funcSymbolDesc := classAst.getFuncDefRef(methodAst, ast.Call.FuncName, ast.Call.FuncProvider)
	if len(funcSymbolDesc.FuncParamsSymbolDesc) != len(ast.Call.Params) {
		return makeSemanticError("func %s.%s expected %d arguments but real %d arguments", ast.Call.FuncProvider,
			ast.Call.FuncName, len(funcSymbolDesc.FuncParamsSymbolDesc), len(ast.Call.Params))
	}
	for i, expr := range ast.Call.Params {
		realType, err := classAst.getAndCheckExpressionType(methodAst, expr)
		if err != nil {
			return err
		}
		expectType := funcSymbolDesc.FuncParamsSymbolDesc[i].variableType
		if !classAst.checkMatch1(&expectType, realType) {
			return makeSemanticError("expected %+v type but real %+v type at func %s.%s %d-th param", expectType,
				realType, ast.Call.FuncProvider, ast.Call.FuncName, i)
		}
	}
	return nil
}

func (classAst *ClassAst) typeCheckReturnStatement(methodAst *ClassFuncOrMethodAst, ast *ReturnStatementAst) error {
	returnTP, err := classAst.getAndCheckExpressionType(methodAst, ast.Return[0])
	if err != nil {
		return err
	}
	if !classAst.checkMatch2(methodAst.ReturnTP, returnTP) {
		return makeSemanticError("func %s return type doesn't match, expected: %+v but real: %+v",
			methodAst.FuncName, methodAst.ReturnTP, returnTP)
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
	rightT, err := classAst.getAndCheckExpressionType0(methodAst, expr.RightExpr)
	if err != nil {
		return nil, err
	}
	tp, err := classAst.checkMatch0(leftT, rightT, expr.Op)
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
	case StringConstantTermType:
		exprTerm.TP = &VariableType{TP: StringType}
	case KeyWordConstantTrueTermType, KeyWordConstantFalseTermType:
		exprTerm.TP = &VariableType{TP: BooleanVariableType}
	case KeyWordConstantThisTermType:
		exprTerm.TP = &VariableType{TP: ClassVariableType, Name: "this"}
	case KeyWordConstantNullTermType:
		exprTerm.TP = &VariableType{TP: ClassVariableType, Name: "null"}
	case VarNameExpressionTermType:
		exprTerm.TP = classAst.getVariableType(methodAst, exprTerm.Value.(string))
	case ArrayIndexExpressionTermType, SubExpressionTermType, UnaryTermExpressionTermType:
		t, err = classAst.getAndCheckExpressionType(methodAst, exprTerm.Value.(*ExpressionAst))
		if err != nil {
			return nil, err
		}
		exprTerm.TP = t
	case SubRoutineCallTermType:
		funDef := classAst.getFuncDefRef(methodAst, exprTerm.Value.(*CallAst).FuncName, exprTerm.Value.(*CallAst).FuncProvider)
		exprTerm.TP = &funDef.FuncReturnSymbolDesc.variableType
	}
	err = classAst.checkTypeOnUnaryOp(t, exprTerm.UnaryOp)
	if err != nil {
		return nil, err
	}
	return t, nil
}

func (classAst *ClassAst) getVariableType(methodAst *ClassFuncOrMethodAst, varName string) *VariableType {
	// Find variable type on class variable first.
	classSymbolTable := symbolTable[classAst.className]
	varDesc, ok := classSymbolTable.VariablesSymbolTable[varName]
	if ok {
		return &varDesc.variableType
	}
	symbolsInFunc, _ := classSymbolTable.FuncSymbolTable[methodAst.FuncName]
	varDesc, ok = symbolsInFunc.FuncParamsSymbolDescMap[varName]
	if ok {
		return &varDesc.variableType
	}
	return &symbolsInFunc.FuncLocalVariableDesc[varName].variableType
}

func (classAst *ClassAst) getVariableAstType(methodAst *ClassFuncOrMethodAst, varAst *VariableAst) (*VariableType, error) {
	// First get variable type
	varType := classAst.getVariableType(methodAst, varAst.VarName)
	if varAst.ArrayIndex != nil && varType.Name != "Array" {
		return nil, makeSemanticError("var %s is not array", varAst.VarName)
	}
	return varType, nil
}

func (classAst *ClassAst) checkMatch0(l, r *VariableType, op *OpAst) (*VariableType, error) {
	if op.Op == AndOpTP || op.Op == OrOpTP {
		return l, checkBooleanMatch(l, r)
	}
	if op.Op == ArrayIndexOpTP {
		if l.TP != ClassVariableType || l.Name != "Array" {
			return nil, makeSemanticError("l: %+v doesn't support index", l)
		}
		if r.TP != CharVariableType || r.TP != IntVariableType {
			return nil, makeSemanticError("unsupported %+v type as array index", r)
		}
		return &VariableType{TP: ClassVariableType, Name: "",}, nil
	}
	// Otherwise l and r should be both a integer compatible type
	if l.TP != IntVariableType || l.TP != CharVariableType || r.TP != IntVariableType || r.TP != CharVariableType {
		return nil, makeSemanticError("unmatched type: l: %+v, r: %+v, op: %+v", l, r, op)
	}
	if l.TP == IntVariableType {
		return l, nil
	}
	if r.TP == IntVariableType {
		return r, nil
	}
	return l, nil
}

func checkBooleanMatch(l, r *VariableType) error {
	if l.TP == BooleanVariableType && r.TP == BooleanVariableType {
		return nil
	}
	return makeSemanticError("unmatched type l: %+v, r: %+v", l, r)
}

func (classAst *ClassAst) checkMatch1(expected, real *VariableType) bool {
	if real == nil {
		return false
	}
	switch expected.TP {
	case BooleanVariableType:
		return real.TP == BooleanVariableType
	case CharVariableType:
		return real.TP == CharVariableType || real.TP == IntVariableType
	case IntVariableType:
		return real.TP == IntVariableType
	case StringType:
		return real.TP == StringType || (real.TP == ClassVariableType && (real.Name == "String" || real.Name == "null"))
	case ClassVariableType:
		if expected.Name == "String" {
			return real.TP == ClassVariableType && (real.Name == "String" || real.Name == "null")
		}
		if expected.Name == "Array" {
			return real.TP == ClassVariableType && (real.Name == "Array" || real.Name == "null")
		}
		return expected.Name == real.Name
	case ArrayType:
		return real.TP == ClassVariableType && real.Name == "Array"
	}
	return false
}

func (classAst *ClassAst) checkMatch2(expected ReturnType, real *VariableType) bool {
	if expected.TP == VoidReturnType {
		return real == nil
	}
	if real == nil {
		return false
	}
	switch expected.TP {
	case BooleanReturnType:
		return real.TP == BooleanVariableType
	case CharReturnType:
		return real.TP == CharVariableType || real.TP == IntVariableType
	case IntReturnType:
		return real.TP == IntVariableType
	case ClassReturnType:
		return real.TP == ClassVariableType && (real.Name == "null" || real.Name == expected.Name)
	}
	return false
}

func (classAst *ClassAst) checkTypeOnUnaryOp(t *VariableType, op *OpAst) error {
	if op == nil {
		return nil
	}
	switch op.Op {
	case NegationOpTP:
		if t.TP != IntVariableType && t.TP != CharVariableType {
			return makeSemanticError("unsupported %+v type on negative", t)
		}
	case BooleanNegationOpTP:
		if t.TP != BooleanVariableType {
			return makeSemanticError("unsupported %+v type on boolean negative", t)
		}
	}
	return nil
}

func methodReturnAnalysisOnClasses(asts []*ClassAst) error {
	for _, ast := range asts {
		err := ast.methodReturnAnalysisOnClass()
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) methodReturnAnalysisOnClass() error {
	for _, method := range classAst.classFuncOrMethod {
		err := classAst.methodReturnAnalysis(method)
		if err != nil {
			return err
		}
	}
	return nil
}

// Check those func who have return a non-void return type, should as least have a return.
func (classAst *ClassAst) methodReturnAnalysis(method *ClassFuncOrMethodAst) error {
	hasReturn, err := classAst.statementReturnAnalysis(method, method.FuncBody)
	if err != nil {
		return err
	}
	if !hasReturn && method.ReturnTP.TP != VoidReturnType {
		return makeSemanticError("method %s doens't have return", method.FuncName)
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
		return true, makeSemanticError("unreachable code")
	}
	return true, nil
}

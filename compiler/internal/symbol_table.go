package internal

import (
	"errors"
	"fmt"
)

type SymbolTableMap map[string]*ClassSymbolTable

var symbolTable SymbolTableMap = map[string]*ClassSymbolTable{}

func (table SymbolTableMap) initStandardLibrary() {
	table.addStandardClassFuncs(
		"Math",
		[]string{"init", "abs", "multiply", "divide", "min", "max", "sqrt"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: IntVariableType}, {TP: IntVariableType}, {TP: IntVariableType},
			{TP: IntVariableType}, {TP: IntVariableType}, {TP: IntVariableType},
		},
		[]int{0, 1, 2, 2, 2, 2, 1},
	)
	table.addStandardClassFuncs(
		"String",
		[]string{
			"new", "dispose", "length", "charAt", "setCharAt", "appendChar", "eraseLastChar", "intValue", "setInt",
			"backSpace", "doubleQuote", "newLine",
		},
		[]SymbolType{
			ClassConstructorSymbolType, ClassMethodSymbolType, ClassMethodSymbolType, ClassMethodSymbolType, ClassMethodSymbolType,
			ClassMethodSymbolType, ClassMethodSymbolType, ClassMethodSymbolType, ClassMethodSymbolType, ClassFuncSymbolType,
			ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: ClassVariableType, Name: "String"}, {TP: VoidVariableType}, {TP: IntVariableType}, {TP: CharVariableType},
			{TP: VoidVariableType}, {TP: ClassVariableType, Name: "String"}, {TP: VoidVariableType}, {TP: IntVariableType},
			{TP: VoidVariableType}, {TP: CharVariableType}, {TP: CharVariableType}, {TP: CharVariableType},
		},
		[]int{0, 0, 0, 1, 2, 1, 0, 0, 1, 0, 0, 0},
	)
	table.addStandardClassFuncs(
		"Array",
		[]string{"new", "dispose"},
		[]SymbolType{ClassFuncSymbolType, ClassMethodSymbolType},
		[]VariableType{
			{TP: ClassVariableType, Name: "Array"}, {TP: VoidVariableType},
		},
		[]int{1, 0},
	)
	table.addStandardClassFuncs(
		"Output",
		[]string{"init", "moveCursor", "printChar", "printString", "printInt", "println", "backSpace"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType},
			{TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType},
		},
		[]int{0, 2, 1, 1, 1, 0, 0},
	)
	table.addStandardClassFuncs(
		"Screen",
		[]string{"init", "clearScreen", "setColor", "drawPixel", "drawLine", "drawRectangle", "drawCircle"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType},
			{TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType},
		},
		[]int{0, 0, 1, 2, 4, 4, 3},
	)
	table.addStandardClassFuncs(
		"Keyboard",
		[]string{"init", "keyPressed", "readChar", "readLine", "readInt"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: CharVariableType}, {TP: CharVariableType}, {TP: ClassVariableType, Name: "string"},
			{TP: IntVariableType},
		},
		[]int{0, 0, 0, 1, 1},
	)
	table.addStandardClassFuncs(
		"Memory",
		[]string{"init", "peek", "poke", "alloc", "deAlloc"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: IntVariableType}, {TP: VoidVariableType}, {TP: ClassVariableType, Name: "Array"},
			{TP: VoidVariableType},
		},
		[]int{0, 1, 2, 1, 1},
	)
	table.addStandardClassFuncs(
		"Sys",
		[]string{"init", "halt", "error", "wait"},
		[]SymbolType{
			ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType, ClassFuncSymbolType,
		},
		[]VariableType{
			{TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType}, {TP: VoidVariableType},
		},
		[]int{0, 0, 1, 1},
	)
}

func (table SymbolTableMap) addStandardClassFuncs(className string, funcNames []string, funcTypes []SymbolType,
	returnTP []VariableType, paramSize []int) {
	classSymbolTable := &ClassSymbolTable{ClassName: className, FuncSymbolTable: map[string]*FuncSymbolTable{}}
	for i, funName := range funcNames {
		fn := &FuncSymbolTable{
			classSymbolTable:     classSymbolTable,
			FuncParamsSymbolDesc: make([]*SymbolDesc, paramSize[i]),
		}
		fnDesc := &SymbolDesc{
			funcSymbolTable:  fn,
			classSymbolTable: classSymbolTable,
			name:             funName,
			symbolType:       funcTypes[i],
			returnType:       returnTP[i],
		}
		fnReturnDesc := &SymbolDesc{
			funcSymbolTable:  fn,
			classSymbolTable: classSymbolTable,
			symbolType:       FuncReturnType,
			returnType:       returnTP[i],
		}
		fn.FuncSymbolDesc, fn.FuncReturnSymbolDesc = fnDesc, fnReturnDesc
		classSymbolTable.FuncSymbolTable[funName] = fn
	}
	table[className] = classSymbolTable
}

type ClassSymbolTable struct {
	ClassName            string
	VariablesSymbolTable map[string]*SymbolDesc
	FuncSymbolTable      map[string]*FuncSymbolTable
	ClassVariableIndex   int
	FuncVariableIndex    int
}

type SymbolDesc struct {
	funcSymbolTable  *FuncSymbolTable
	classSymbolTable *ClassSymbolTable
	name             string
	symbolType       SymbolType   // Represent whether the symbol is a static, object variable, or func, method, constructor and so on.
	variableType     VariableType // Some identifier can doesn't have variableType, like className, funcName.
	index            int
	returnType       VariableType
}

type FuncSymbolTable struct {
	// Func related symbol desc, like: funcName, symbolType, etc.
	classSymbolTable        *ClassSymbolTable
	FuncSymbolDesc          *SymbolDesc
	FuncParamsSymbolDesc    []*SymbolDesc
	FuncParamsSymbolDescMap map[string]*SymbolDesc
	FuncReturnSymbolDesc    *SymbolDesc
	FuncLocalVariableDesc   map[string]*SymbolDesc
	paramIndicator          int // used for param index
	localVarIndicator       int // used for local variable
}

type SymbolType int

const (
	ClassNameSymbolType SymbolType = iota
	ClassStaticVariableSymbolType
	ClassVariableSymbolType
	ClassConstructorSymbolType
	// Method.
	ClassMethodSymbolType
	// Static func
	ClassFuncSymbolType
	// The remaining are function scope symbols.
	FuncParamType
	FuncVariableType
	FuncReturnType
)

var classStaticVariableIndicator = 0

func (table SymbolTableMap) lookUpClass(className string) *ClassSymbolTable {
	return symbolTable[className]
}

func (table SymbolTableMap) lookUpFuncInClass(className, funcName string) *FuncSymbolTable {
	classSymbolTable := table.lookUpClass(className)
	if classSymbolTable == nil {
		return nil
	}
	return classSymbolTable.FuncSymbolTable[funcName]
}

func (table SymbolTableMap) lookUpStaticFuncInClass(className, funcName string) *FuncSymbolTable {
	fn := table.lookUpFuncInClass(className, funcName)
	if fn == nil {
		return nil
	}
	if fn.FuncSymbolDesc.symbolType != ClassFuncSymbolType {
		return nil
	}
	return fn
}

func (table SymbolTableMap) lookUpNonStaticFuncInClass(className, funcName string) *FuncSymbolTable {
	fn := table.lookUpFuncInClass(className, funcName)
	if fn == nil {
		return nil
	}
	if fn.FuncSymbolDesc.symbolType == ClassFuncSymbolType {
		return nil
	}
	return fn
}

// Looking up variable in current func. Note because this variable also can be a static variable of
// a class.
func (table SymbolTableMap) lookUpVarInFunc(className, funcName, varName string) (*SymbolDesc, error) {
	symbolsInFunc := table.lookUpFuncInClass(className, funcName)
	if symbolsInFunc == nil {
		return nil, makeSemanticError("cannot find such variable %s and such func %s.%s", varName, className, funcName)
	}
	// Func params
	ret, ok := symbolsInFunc.FuncParamsSymbolDescMap[varName]
	if ok {
		return ret, nil
	}
	// Or func local variables.
	ret, ok = symbolsInFunc.FuncLocalVariableDesc[varName]
	if ok {
		return ret, nil
	}
	// Or class variables.
	ret = table.lookUpClassVar(className, varName)
	if ret == nil {
		return nil, nil
	}
	// If this function is a static function, then we cannot use classVariable here.
	if symbolsInFunc.FuncSymbolDesc.symbolType == ClassFuncSymbolType && ret.symbolType == ClassVariableSymbolType {
		return nil, makeSemanticError("cannot use non static variable %s in static func %s.%s",
			varName, className, funcName)
	}
	return ret, nil
}

func (table SymbolTableMap) lookUpClassVar(className string, varName string) *SymbolDesc {
	classSymbol := table.lookUpClass(className)
	if classSymbol == nil {
		return nil
	}
	return classSymbol.VariablesSymbolTable[varName]
}

func (table SymbolTableMap) isClassNameExist(className string) bool {
	return symbolTable[className] != nil
}

// There is a call in func $funcName at class $className. we will return the corresponding funcSymbolTable of the
// refer func, also we did some semantic checking here.
func (table SymbolTableMap) lookUpFuncInSpecificClassAndFunc(className string, funcName string, callAst *CallAst) (*FuncSymbolTable, error) {
	funcProvider := callAst.FuncProvider
	currentFunc := table.lookUpFuncInClass(className, funcName)
	// If funcProvider is this.
	if funcProvider == "this" {
		// If current method is a static method, we don't allow use this here.
		if currentFunc.FuncSymbolDesc.symbolType == ClassFuncSymbolType {
			return nil, makeSemanticError("cannot use this in static method %s.%s.", className, funcName)
		}
		callerFn := table.lookUpFuncInClass(className, callAst.FuncName)
		if callerFn.FuncSymbolDesc.symbolType == ClassFuncSymbolType || callerFn.FuncSymbolDesc.symbolType ==
			ClassConstructorSymbolType {
			return nil, makeSemanticError("cannot use this call static or constructor method %s",
				callAst.FuncName)
		}
		return callerFn, nil
	}
	// If funcProvider is "", we try to find class in current class.
	if funcProvider == "" {
		callFn := symbolTable.lookUpFuncInClass(className, callAst.FuncName)
		if callFn == nil {
			return nil, makeSemanticError("cannot find such func %s at %s.%s", callAst.FuncName, className, funcName)
		}
		// We don't allow in a static method, call non-static method.
		if currentFunc.FuncSymbolDesc.symbolType == ClassFuncSymbolType && callFn.FuncSymbolDesc.symbolType ==
			ClassMethodSymbolType {
			return nil, makeSemanticError("static method %s.%s cannot call non-static method %s.%s", className, funcName,
				className, callAst.FuncName)
		}
		return callFn, nil
	}
	// If funcProvider is a variable name in currentFn.
	varSymbolTable, err := table.lookUpVarInFunc(className, funcName, funcProvider)
	if err != nil {
		return nil, err
	}
	if varSymbolTable != nil {
		// Then we can find the method of this variable.
		if varSymbolTable.variableType.TP != ClassVariableType {
			return nil, makeSemanticError("var %s doesn't have such method %s at %s.%s", funcProvider, callAst.FuncName,
				className, funcProvider)
		}
		// Check whether this variable has such method.
		calledFn := symbolTable.lookUpFuncInClass(varSymbolTable.variableType.Name, callAst.FuncName)
		if calledFn == nil {
			return nil, makeSemanticError("var %s doesn't have such func %s at %s.%s", funcProvider, callAst.FuncName, className,
				funcName)
		}
		if calledFn.FuncSymbolDesc.symbolType == ClassConstructorSymbolType || calledFn.FuncSymbolDesc.symbolType ==
			ClassFuncSymbolType {
			return nil, makeSemanticError("var %s cannot call static method %s at %s.%s", funcProvider, callAst.FuncName, className,
				funcName)
		}
		return calledFn, nil
	}
	// Then funcProvider can be a className, and the corresponding method can be a static method of that class.
	fnSymbol := table.lookUpFuncInClass(callAst.FuncProvider, callAst.FuncName)
	if fnSymbol == nil {
		return nil, makeSemanticError("cannot find such func %s.%s at %s.%s", callAst.FuncProvider, callAst.FuncName, className, funcName)
	}
	if fnSymbol.FuncSymbolDesc.symbolType == ClassMethodSymbolType {
		return nil, makeSemanticError("object method %s.%s must be called by an object at %s.%s", callAst.FuncProvider, callAst.FuncName,
			className, funcName)
	}
	// Then because callFn must be a static method or constructor. so we allow it.
	return fnSymbol, nil
}

// If we encounter some variables, some declaration declared twice, then will return err.
func (table SymbolTableMap) buildSymbolTables(asts []*ClassAst) error {
	for _, ast := range asts {
		classSymbolTable, err := buildClassSymbolTable(ast)
		if err != nil {
			return err
		}
		_, ok := table[ast.className]
		if ok {
			return makeSemanticError("found duplicate className: %s", ast.className)
		}
		table[classSymbolTable.ClassName] = classSymbolTable
	}
	return nil
}

func buildClassSymbolTable(ast *ClassAst) (*ClassSymbolTable, error) {
	classSymbolTable := &ClassSymbolTable{
		ClassName:            ast.className,
		VariablesSymbolTable: make(map[string]*SymbolDesc),
		FuncSymbolTable:      make(map[string]*FuncSymbolTable),
	}
	ast.classSymbolTable = classSymbolTable
	err := classSymbolTable.buildClassVariables(ast)
	if err != nil {
		return nil, err
	}
	err = classSymbolTable.buildClassMethods(ast)
	if err != nil {
		return nil, err
	}
	return classSymbolTable, nil
}

// Build variables table.
func (classSymbolTable *ClassSymbolTable) buildClassVariables(ast *ClassAst) error {
	if len(ast.classVariables) <= 0 {
		return nil
	}
	for _, variable := range ast.classVariables {
		err := classSymbolTable.buildVariable(variable)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildVariable(classVariableAst *ClassVariableAst) error {
	classVariableSymbolTable := classSymbolTable.VariablesSymbolTable
	_, ok := classVariableSymbolTable[classVariableAst.VariableName]
	if ok {
		return makeSemanticError("duplicate variable name: %s on class %s", classVariableAst.VariableName, classSymbolTable.ClassName)
	}
	variableSymbolDesc := &SymbolDesc{
		// Link to which class this variable belongs to.
		classSymbolTable: classSymbolTable,
		name:             classVariableAst.VariableName,
		variableType:     classVariableAst.VariableType,
		// Note: variable has same size memory layout. This won't happen in real programming language.
	}
	if classVariableAst.FieldTP == ObjectFieldType {
		variableSymbolDesc.index, variableSymbolDesc.symbolType = classSymbolTable.ClassVariableIndex,
			ClassVariableSymbolType
		classSymbolTable.ClassVariableIndex++
	} else {
		variableSymbolDesc.index, variableSymbolDesc.symbolType = classStaticVariableIndicator,
			ClassStaticVariableSymbolType
		classStaticVariableIndicator++
	}
	classVariableSymbolTable[classVariableAst.VariableName] = variableSymbolDesc
	classVariableAst.symbolDesc = variableSymbolDesc
	return nil
}

// Build methods table.
func (classSymbolTable *ClassSymbolTable) buildClassMethods(ast *ClassAst) error {
	if len(ast.classFuncOrMethod) <= 0 {
		return nil
	}
	for _, method := range ast.classFuncOrMethod {
		err := classSymbolTable.buildMethod(method)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildMethod(methodAst *ClassFuncOrMethodAst) error {
	return classSymbolTable.buildMethod0(methodAst)
}

// In language like java. the constructor has different semantics than other methods. But in jack, we didn't
// consider this.
func (classSymbolTable *ClassSymbolTable) buildMethod0(methodAst *ClassFuncOrMethodAst) error {
	methodType := ClassConstructorSymbolType
	switch methodAst.FuncTP {
	case ClassConstructorType:
		methodType = ClassConstructorSymbolType
	case ClassMethodType:
		methodType = ClassMethodSymbolType
	case ClassFuncType:
		methodType = ClassFuncSymbolType
	}
	methodName := methodAst.FuncName
	_, ok := classSymbolTable.FuncSymbolTable[methodName]
	if ok {
		return makeSemanticError("duplicate funcName: %s at class %s", methodName, classSymbolTable.ClassName)
	}
	funcSymbolTable := new(FuncSymbolTable)
	funcSymbolTable.classSymbolTable = classSymbolTable
	funcSymbolTable.FuncSymbolDesc = &SymbolDesc{
		classSymbolTable: classSymbolTable,
		funcSymbolTable:  funcSymbolTable,
		name:             methodName,
		symbolType:       methodType,
	}
	err := classSymbolTable.buildFuncParams(methodAst, funcSymbolTable)
	if err != nil {
		return err
	}
	err = classSymbolTable.buildFuncLocalVariableDesc(methodAst, funcSymbolTable)
	if err != nil {
		return err
	}
	err = classSymbolTable.buildFuncReturnDesc(methodAst.ReturnTP, funcSymbolTable)
	if err != nil {
		return err
	}
	classSymbolTable.FuncSymbolTable[methodName] = funcSymbolTable
	methodAst.funcSymbol = funcSymbolTable
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildFuncParams(methodAst *ClassFuncOrMethodAst, funcSymbolTable *FuncSymbolTable) error {
	if methodAst.FuncTP == ClassConstructorType || methodAst.FuncTP == ClassMethodType {
		funcSymbolTable.paramIndicator++
	}
	funcParamsSymbolDescList := make([]*SymbolDesc, 0, len(methodAst.Params))
	funcParamsSymbolDescMap := map[string]*SymbolDesc{}
	for _, param := range methodAst.Params {
		_, ok := funcParamsSymbolDescMap[param.ParamName]
		if ok {
			return makeSemanticError("duplicate funcParam %s at func %s.%s", param.ParamName,
				classSymbolTable.ClassName, funcSymbolTable.FuncSymbolDesc.name)
		}
		symbolDesc := &SymbolDesc{
			funcSymbolTable:  funcSymbolTable,
			classSymbolTable: classSymbolTable,
			name:             param.ParamName,
			symbolType:       FuncParamType,
			variableType:     param.ParamTP,
			index:            funcSymbolTable.paramIndicator,
		}
		funcSymbolTable.paramIndicator++
		funcParamsSymbolDescMap[param.ParamName] = symbolDesc
		funcParamsSymbolDescList = append(funcParamsSymbolDescList, symbolDesc)
		param.symbolDesc = symbolDesc
	}
	funcSymbolTable.FuncParamsSymbolDesc, funcSymbolTable.FuncParamsSymbolDescMap = funcParamsSymbolDescList,
		funcParamsSymbolDescMap
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildFuncLocalVariableDesc(methodAst *ClassFuncOrMethodAst, funcSymbolTable *FuncSymbolTable) error {
	funcLocalVariableDesc := map[string]*SymbolDesc{}
	funcSymbolTable.FuncLocalVariableDesc = funcLocalVariableDesc
	for _, statement := range methodAst.FuncBody {
		funcLocalVarDesc, err := classSymbolTable.buildLocalVarDeclareStatement(statement, funcSymbolTable)
		if err != nil {
			return err
		}
		err = classSymbolTable.addFuncLocalVarDescs(funcSymbolTable, funcLocalVarDesc)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classSymbolTable *ClassSymbolTable) addFuncLocalVarDescs(funcSymbolTable *FuncSymbolTable, funcLocalVarDesc []*SymbolDesc) error {
	for _, funcVarDesc := range funcLocalVarDesc {
		_, ok := funcSymbolTable.FuncLocalVariableDesc[funcVarDesc.name]
		if ok {
			return makeSemanticError("duplicate var %s at %s.%s", funcVarDesc.name,
				classSymbolTable.ClassName, funcSymbolTable.FuncSymbolDesc.name)
		}
		_, ok = funcSymbolTable.FuncParamsSymbolDescMap[funcVarDesc.name]
		if ok {
			return makeSemanticError("duplicate var %s at %s.%s", funcVarDesc.name,
				classSymbolTable.ClassName, funcSymbolTable.FuncSymbolDesc.name)
		}
		funcSymbolTable.FuncLocalVariableDesc[funcVarDesc.name] = funcVarDesc
	}
	return nil
}

// In jack, variable declaration is appeared before if, while ... other statements.
func (classSymbolTable *ClassSymbolTable) buildLocalVarDeclareStatement(statementAst *StatementAst, funcSymbolTable *FuncSymbolTable) (descs []*SymbolDesc, err error) {
	if statementAst.StatementTP != VariableDeclareStatementTP {
		return nil, nil
	}
	varDeclareAst := statementAst.Statement.(*VarDeclareAst)
	varSymbolDesc := &SymbolDesc{
		funcSymbolTable:  funcSymbolTable,
		classSymbolTable: classSymbolTable,
		name:             varDeclareAst.VarName,
		symbolType:       FuncVariableType,
		variableType:     varDeclareAst.VarType,
		index:            funcSymbolTable.localVarIndicator,
	}
	descs = append(descs, varSymbolDesc)
	funcSymbolTable.localVarIndicator++
	varDeclareAst.symbolDesc = varSymbolDesc
	return
}

func (classSymbolTable *ClassSymbolTable) buildFuncReturnDesc(returnTp VariableType, funcSymbolTable *FuncSymbolTable) error {
	funcSymbolTable.FuncReturnSymbolDesc = &SymbolDesc{
		classSymbolTable: classSymbolTable,
		funcSymbolTable:  funcSymbolTable,
		symbolType:       FuncReturnType,
		returnType:       returnTp,
	}
	return nil
}

func makeSemanticError(format string, msg ...interface{}) error {
	return errors.New(fmt.Sprintf(format, msg...))
}

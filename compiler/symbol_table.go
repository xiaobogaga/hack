package compiler

import (
	"errors"
	"fmt"
)

type SymbolTableMap map[string]*ClassSymbolTable

var symbolTable SymbolTableMap

type ClassSymbolTable struct {
	ClassName            string
	VariablesSymbolTable map[string]*SymbolDesc
	FuncSymbolTable      map[string]*SymbolsInFunc
	ClassVariableIndex   int
	FuncVariableIndex    int
}

func (table SymbolTableMap) lookUpClass(className string) *ClassSymbolTable {
	return symbolTable[className]
}

func (table SymbolTableMap) lookUpFuncInClass(className, funcName string) *SymbolsInFunc {
	classSymbolTable := table.lookUpClass(className)
	if classSymbolTable == nil {
		return nil
	}
	return classSymbolTable.FuncSymbolTable[funcName]
}

func (table SymbolTableMap) lookUpVarInFunc(className, funcName, varName string) *SymbolDesc {
	symbolsInFunc := table.lookUpFuncInClass(className, funcName)
	if symbolsInFunc == nil {
		return nil
	}
	ret, ok := symbolsInFunc.FuncParamsSymbolDescMap[varName]
	if ok {
		return ret
	}
	return symbolsInFunc.FuncLocalVariableDesc[varName]
}

func (table SymbolTableMap) lookUpClassVar(className string, varName string) *SymbolDesc {
	classSymbol := table.lookUpClass(className)
	if classSymbol == nil {
		return nil
	}
	return classSymbol.VariablesSymbolTable[varName]
}

type SymbolDesc struct {
	name         string
	symbolType   SymbolType
	variableType VariableType // Some identifier can doesn't have variableType, like className, funcName.
	index        int
	returnType   ReturnType
}

type SymbolsInFunc struct {
	// Func related symbol desc, like: funcName, symbolType, etc.
	ClassName               string
	FuncSymbolDesc          *SymbolDesc
	FuncParamsSymbolDesc    []*SymbolDesc
	FuncParamsSymbolDescMap map[string]*SymbolDesc
	FuncReturnSymbolDesc    *SymbolDesc
	FuncLocalVariableDesc   map[string]*SymbolDesc
}

type SymbolType int

const (
	ClassNameSymbolType SymbolType = iota
	ClassStaticVariableSymbolType
	ClassVariableSymbolType
	ClassConstructorSymbolType
	ClassMethodSymbolType
	ClassFuncSymbolType
	// The remaining are function scope symbols.
	FuncParamType
	FuncVariableType
	FuncReturnType
)

// If we encounter some variables, some declaration declared twice, then will return err.
func buildSymbolTables(asts []*ClassAst) error {
	for _, ast := range asts {
		classSymbolTable, err := buildClassSymbolTable(ast)
		if err != nil {
			return err
		}
		_, ok := symbolTable[ast.className]
		if ok {
			// Todo: we need to add error location information, but now we don't bother to do this.
			return makeSemanticError("duplicate className: %s", ast.className)
		}
		symbolTable[classSymbolTable.ClassName] = classSymbolTable
	}
	return nil
}

func buildClassSymbolTable(ast *ClassAst) (*ClassSymbolTable, error) {
	classSymbolTable := &ClassSymbolTable{
		ClassName:            ast.className,
		VariablesSymbolTable: make(map[string]*SymbolDesc),
		FuncSymbolTable:      make(map[string]*SymbolsInFunc),
	}
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
	for _, name := range classVariableAst.VariableNames {
		_, ok := classVariableSymbolTable[name]
		if ok {
			return makeSemanticError("duplicate variable name: %s", name)
		}
		variableSymbolDesc := &SymbolDesc{
			name:         name,
			symbolType:   ClassStaticVariableSymbolType,
			variableType: classVariableAst.VariableType,
			// Here variable has same size memory layout. This won't happen in real programming language.
			index: classSymbolTable.ClassVariableIndex,
		}
		classSymbolTable.ClassVariableIndex++
		if classVariableAst.FieldTP == ObjectFieldType {
			variableSymbolDesc.symbolType = ClassVariableSymbolType
		}
		classVariableSymbolTable[name] = variableSymbolDesc
	}
	return nil
}

// Build methods table.
func (classSymbolTable *ClassSymbolTable) buildClassMethods(ast *ClassAst) error {
	for _, method := range ast.classFuncOrMethod {
		err := classSymbolTable.buildMethod(method)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildMethod(methodAst *ClassFuncOrMethodAst) error {
	switch methodAst.FuncTP {
	case ClassConstructorType:
		return classSymbolTable.buildMethod0(methodAst, ClassConstructorSymbolType)
	case ClassMethodType:
		return classSymbolTable.buildMethod0(methodAst, ClassMethodSymbolType)
	case ClassFuncType:
		return classSymbolTable.buildMethod0(methodAst, ClassFuncSymbolType)
	default:
		// Todo: add err msg
		return makeSemanticError("")
	}
}

// In language like java. the constructor has different semantics than other methods. But in jack, we didn't
// consider this.
func (classSymbolTable *ClassSymbolTable) buildMethod0(methodAst *ClassFuncOrMethodAst, methodSymbolType SymbolType) error {
	methodName := methodAst.FuncName
	_, ok := classSymbolTable.FuncSymbolTable[methodName]
	if ok {
		return makeSemanticError("duplicate funcName: %s", methodName)
	}
	funcSymbolTable := new(SymbolsInFunc)
	funcSymbolTable.FuncSymbolDesc = &SymbolDesc{
		name:       methodName,
		symbolType: methodSymbolType,
	}
	funcParamSymbols, funcParamSymbolMap, err := classSymbolTable.buildFuncParams(methodAst.Params)
	if err != nil {
		return err
	}
	funcSymbolTable.FuncParamsSymbolDesc, funcSymbolTable.FuncParamsSymbolDescMap = funcParamSymbols, funcParamSymbolMap
	funcLocalVariableDesc, err := classSymbolTable.buildFuncLocalVariableDesc(methodAst.FuncBody)
	if err != nil {
		return err
	}
	funcReturnDesc, err := classSymbolTable.buildFuncReturnDesc(methodAst.ReturnTP)
	if err != nil {
		return err
	}
	funcSymbolTable.FuncLocalVariableDesc, funcSymbolTable.FuncReturnSymbolDesc = funcLocalVariableDesc, funcReturnDesc
	funcSymbolTable.ClassName = classSymbolTable.ClassName
	classSymbolTable.FuncSymbolTable[methodName] = funcSymbolTable
	return nil
}

func (classSymbolTable *ClassSymbolTable) buildFuncParams(funcParamsAst []*FuncParamAst) ([]*SymbolDesc, map[string]*SymbolDesc, error) {
	funcParamsSymbolDescList := make([]*SymbolDesc, len(funcParamsAst))
	funcParamsSymbolDescMap := map[string]*SymbolDesc{}
	for _, param := range funcParamsAst {
		_, ok := funcParamsSymbolDescMap[param.ParamName]
		if ok {
			return nil, nil, makeSemanticError("duplicate funcParam name %s", param.ParamName)
		}
		symbolDesc := &SymbolDesc{
			name:         param.ParamName,
			symbolType:   FuncParamType,
			variableType: param.ParamTP,
			index:        0, // Todo: need to set index here?
		}
		funcParamsSymbolDescMap[param.ParamName] = symbolDesc
		funcParamsSymbolDescList = append(funcParamsSymbolDescList, symbolDesc)
	}
	return funcParamsSymbolDescList, funcParamsSymbolDescMap, nil
}

func (classSymbolTable *ClassSymbolTable) buildFuncLocalVariableDesc(funcBody []*StatementAst) (map[string]*SymbolDesc, error) {
	funcLocalVariableDesc := map[string]*SymbolDesc{}
	for _, statement := range funcBody {
		funcVarDescs, err := classSymbolTable.buildStatement(statement)
		if err != nil {
			return nil, err
		}
		err = addFuncVarDescs(funcVarDescs, funcLocalVariableDesc)
		if err != nil {
			return nil, err
		}
	}
	return nil, nil
}

func addFuncVarDescs(funcVarDescs []*SymbolDesc, funcLocalVariableDesc map[string]*SymbolDesc) error {
	for _, funcVarDesc := range funcVarDescs {
		_, ok := funcLocalVariableDesc[funcVarDesc.name]
		if ok {
			// Todo: fill in funcName.
			return makeSemanticError("duplicate var name %s in %s.", funcVarDesc.name)
		}
		funcLocalVariableDesc[funcVarDesc.name] = funcVarDesc
	}
	return nil
}

// In jack, variable declaration is appeared before if, while ... other statements.
func (classSymbolTable *ClassSymbolTable) buildStatement(statementAst *StatementAst) (descs []*SymbolDesc, err error) {
	if statementAst.StatementTP != VariableDeclareStatementTP {
		return nil, nil
	}
	varDeclareAst := statementAst.Statement.(*VarDeclareAst)
	for _, varName := range varDeclareAst.VarNames {
		descs = append(descs, &SymbolDesc{
			name:         varName,
			symbolType:   FuncVariableType,
			variableType: varDeclareAst.VarType,
			// Todo: we need to set index here.
			index: 0,
		})
	}
	return
}

func (classSymbolTable *ClassSymbolTable) buildFuncReturnDesc(returnTp ReturnType) (*SymbolDesc, error) {
	// Todo: do we need to set index here?
	return &SymbolDesc{
		symbolType: FuncReturnType,
		returnType: returnTp,
	}, nil
}

func makeSemanticError(format string, msg ...interface{}) error {
	return errors.New(fmt.Sprintf(format, msg...))
}

package compiler

import (
	"fmt"
	"strconv"
)

var conditionLabel = 0

func generateCodes(classAsts []*ClassAst) error {
	for _, classAst := range classAsts {
		err := classAst.generateCode()
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) generateCode() error {
	for _, variable := range classAst.classVariables {
		if variable.FieldTP == ClassFieldType {
			classAst.generateCodeForClassVariable(variable)
		}
	}
	for _, method := range classAst.classFuncOrMethod {
		err := classAst.generateMethodCode(method)
		if err != nil {
			return err
		}
	}
	return nil
}

func (classAst *ClassAst) generateCodeForClassVariable(variable *ClassVariableAst) {
	classVariable := symbolTable.lookUpClassVar(classAst.className, variable.VariableName)
	classAst.writeOutput(fmt.Sprintf("PUSH STATIC %d", classVariable.index))
}

// Generate func method vm code.
// vm codes:
// function className_funcName len(params) + (if it is a method : 1; 0)
func (classAst *ClassAst) generateMethodCode(method *ClassFuncOrMethodAst) error {
	// For constructor, need to call Memory.alloc() to allocate memories.
	// The jack standard mapping said it recommend us to alloc memory in constructor.
	// But we don't do that, instead, we put this to the caller side, namely, we assume that
	// When a constructor method is called, we let the caller generate such memory and put
	// The location of this variable to the first parameter of method.
	code := fmt.Sprintf("function %s_%s %d", classAst.className, method.FuncName, getFuncLocalParamsLen(method))
	classAst.writeOutput(code)
	classAst.generateStatementsCode(method, method.FuncBody)
	return nil
}

func getFuncLocalParamsLen(method *ClassFuncOrMethodAst) int {
	ret := 0
	for _, statement := range method.FuncBody {
		if statement.StatementTP != VariableDeclareStatementTP {
			continue
		}
		ret += 1
	}
	return ret
}

func (classAst *ClassAst) generateStatementsCode(method *ClassFuncOrMethodAst, statements []*StatementAst) {
	for _, stm := range statements {
		classAst.generateStatementCode(method, stm)
	}
}

func (classAst *ClassAst) generateStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	switch statement.StatementTP {
	case VariableDeclareStatementTP:
		classAst.generateVariableDeclareStatementCode(method, statement)
	case LetStatementTP:
		classAst.generateLetStatementCode(method, statement)
	case IfStatementTP:
		classAst.generateIfStatementCode(method, statement)
	case WhileStatementTP:
		classAst.generateWhileStatementCode(method, statement)
	case DoStatementTP:
		classAst.generateDoStatementCode(method, statement)
	case ReturnStatementTP:
		classAst.generateReturnStatementCode(method, statement)
	default:
		panic("unknown statement tp")
	}
}

func (classAst *ClassAst) generateVariableDeclareStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	// Can ignore
	return
}

// For let statement: letVariable = expression
// generate code for expression first.
func (classAst *ClassAst) generateLetStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	letStatement := statement.Statement.(*LetStatementAst)
	classAst.generateExpressionCode(method, letStatement.Value)
	classAst.generateSaveToVariableCode(method, letStatement.LetVariable)
}

// Current value is already on top.
func (classAst ClassAst) generateSaveToVariableCode(method *ClassFuncOrMethodAst, variable *VariableAst) {
	varSymbol, _ := symbolTable.lookUpVarInFunc(classAst.className, method.FuncName, variable.VarName)
	// Array is special.
	if variable.ArrayIndex != nil {
		switch varSymbol.symbolType {
		case ClassStaticVariableSymbolType:
			classAst.writeOutput(fmt.Sprintf("PUSH STATIC %d", varSymbol.index))
		case ClassVariableSymbolType:
			// If this variable name is a class variable. then it must be a variable of current class.
			// Then we can get the variable address from the first parameter of current method.
			classAst.writeOutput("PUSH ARGUMENT 0")
			classAst.writeOutput(fmt.Sprintf("PUSH CONSTANT %d", varSymbol.index))
			classAst.writeOutput("ADD")
		case FuncParamType:
			classAst.writeOutput(fmt.Sprintf("PUSH ARGUMENT %d", varSymbol.index))
		case FuncVariableType:
			classAst.writeOutput(fmt.Sprintf("PUSH LOCAL %d", varSymbol.index))
		}
		classAst.generateExpressionCode(method, variable.ArrayIndex)
		classAst.writeOutput("ADD")
		classAst.writeOutput("POP POINTER 1")
		classAst.writeOutput("POP THAT 1")
		return
	}
	switch varSymbol.symbolType {
	case ClassStaticVariableSymbolType:
		classAst.writeOutput(fmt.Sprintf("POP STATIC %d", varSymbol.index))
	case ClassVariableSymbolType:
		// If this variable name is a class variable. then it must be a variable of current class.
		// Then we can get the variable address from the first parameter of current method.
		classAst.writeOutput("PUSH ARGUMENT 0")
		classAst.writeOutput(fmt.Sprintf("PUSH CONSTANT %d", varSymbol.index))
		classAst.writeOutput("ADD")
		classAst.writeOutput("POP POINTER 0")
		classAst.writeOutput("POP THIS 0")
	case FuncParamType:
		classAst.writeOutput(fmt.Sprintf("POP ARGUMENT %d", varSymbol.index))
	case FuncVariableType:
		classAst.writeOutput(fmt.Sprintf("POP LOCAL %d", varSymbol.index))
	}
}

// If condition vm code.
// IF-GOTO Else_Label
// If statement vm code.
// GOTO Exit_Label
// Label Else_Label
// Else statement vm code
// Exit_Label
func (classAst *ClassAst) generateIfStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	ifStatement := statement.Statement.(*IfStatementAst)
	classAst.generateExpressionCode(method, ifStatement.Condition)
	// Generate if, else code.
	elseStatementLabel, exitStatementLabel := fmt.Sprintf("else_%d", conditionLabel), fmt.Sprintf("if_exit_%d", conditionLabel+1)
	conditionLabel += 2
	classAst.writeOutput("IF-GOTO " + elseStatementLabel)
	classAst.generateStatementsCode(method, ifStatement.IfTrueStatements)
	classAst.writeOutput("GOTO " + exitStatementLabel)
	classAst.writeOutput("Label " + elseStatementLabel)
	classAst.generateStatementsCode(method, ifStatement.ElseStatements)
	classAst.writeOutput("Label " + exitStatementLabel)
}

// Label while_check_label
// While condition vm code
// IF-GOTO Exit_Label
// Statements vm codes
// GOTO while_check_label
// Label Exit_Label
func (classAst *ClassAst) generateWhileStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	whileStatement := statement.Statement.(*WhileStatementAst)
	whileCheckLabel, exitLabel := fmt.Sprintf("while_check_%d", conditionLabel), fmt.Sprintf("while_exit_%d", conditionLabel+1)
	conditionLabel += 2
	classAst.writeOutput("Label " + whileCheckLabel)
	classAst.generateExpressionCode(method, whileStatement.Condition)
	classAst.writeOutput("IF-GOTO " + exitLabel)
	classAst.generateStatementsCode(method, whileStatement.Statements)
	classAst.writeOutput("GOTO " + whileCheckLabel)
	classAst.writeOutput("Label " + exitLabel)
}

func (classAst *ClassAst) generateDoStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	doStatement := statement.Statement.(*DoStatementAst)
	classAst.generateFuncCallCode(method, doStatement.Call)
}

func (classAst *ClassAst) generateReturnStatementCode(method *ClassFuncOrMethodAst, statement *StatementAst) {
	returnStatement := statement.Statement.(*ReturnStatementAst)
	for _, stm := range returnStatement.Return {
		classAst.generateExpressionCode(method, stm)
	}
	classAst.writeOutput("return")
}

// generateExpressionCode: for example: 5 + a[1] * c.foo(a, b) + name
//
//                      +
//                    /   \
//                   *    name
//                 /  \
//                +     c.foo(a, b)
//              /  \
//             5   call
//           /  \
//          a    1
//
// a postOrder traversal is enough.
func (classAst *ClassAst) generateExpressionCode(method *ClassFuncOrMethodAst, expr *ExpressionAst) {
	if expr.LeftExpr != nil {
		classAst.generateExpressionTermOrExpressionCode(method, expr.LeftExpr)
	}
	if expr.RightExpr != nil {
		classAst.generateExpressionTermOrExpressionCode(method, expr.RightExpr)
	}
	if expr.Op != nil {
		classAst.generateOpCode(method, expr.Op)
	}
}

func (classAst *ClassAst) generateExpressionTermOrExpressionCode(method *ClassFuncOrMethodAst, expr interface{}) {
	exprTerm, ok := expr.(*ExpressionTerm)
	if ok {
		classAst.generateExpressionTermCode(method, exprTerm)
		return
	}
	classAst.generateExpressionCode(method, expr.(*ExpressionAst))
}

func (classAst *ClassAst) generateExpressionTermCode(method *ClassFuncOrMethodAst, exprTerm *ExpressionTerm) {
	switch exprTerm.Type {
	case IntegerConstantTermType:
		classAst.writeOutput(fmt.Sprintf("PUSH CONSTANT %d", exprTerm.Value.(int)))
	case CharacterConstantTermType:
		v, _ := strconv.Atoi(exprTerm.Value.(string))
		classAst.writeOutput(fmt.Sprintf("PUSH CONSTANT %d", v))
	case StringConstantTermType:
		classAst.generateConstantStringCode(exprTerm.Value.(string))
	case KeyWordConstantFalseTermType, KeyWordConstantNullTermType:
		classAst.writeOutput("PUSH CONSTANT 0")
	case KeyWordConstantTrueTermType:
		classAst.writeOutput("PUSH CONSTANT 1")
		classAst.writeOutput("NEG")
	case KeyWordConstantThisTermType:
		classAst.writeOutput("PUSH ARGUMENT 0")
	case VarNameExpressionTermType:
		classAst.generateVarNameCode(method, exprTerm.Value.(*VariableAst).VarName)
	case ArrayIndexExpressionTermType:
		classAst.generateArrayIndexCode(method, exprTerm.Value.(*ExpressionAst))
	case SubRoutineCallTermType:
		classAst.generateFuncCallCode(method, exprTerm.Value.(*CallAst))
	case SubExpressionTermType:
		classAst.generateExpressionCode(method, exprTerm.Value.(*ExpressionAst))
	case UnaryTermExpressionTermType:
		classAst.generateExpressionTermCode(method, exprTerm.Value.(*ExpressionTerm))
	}
	classAst.generateUnaryOpCode(method, exprTerm.UnaryOp)
}

func (classAst *ClassAst) generateFuncCallCode(method *ClassFuncOrMethodAst, callAst *CallAst) {
	funcDesc, _ := symbolTable.lookUpFuncInSpecificClassAndFunc(classAst.className, method.FuncName, callAst)
	if funcDesc.FuncSymbolDesc.symbolType == ClassConstructorSymbolType {
		classAst.writeOutput(fmt.Sprintf("PUSH %d", symbolTable.lookUpClass(funcDesc.classSymbolTable.ClassName).ClassVariableIndex))
		classAst.writeOutput("CALL Memory.alloc 1")
	}
	if funcDesc.FuncSymbolDesc.symbolType == ClassFuncSymbolType {
		// Push the base memory address of the caller to stack.
		classAst.generateVarNameCode(method, callAst.FuncProvider)
	}
	for _, param := range callAst.Params {
		classAst.generateExpressionTermOrExpressionCode(method, param)
	}
	classAst.writeOutput(fmt.Sprintf("CALL %s_%s %d", funcDesc.classSymbolTable.ClassName, funcDesc.FuncSymbolDesc.name, len(funcDesc.FuncParamsSymbolDesc)))
	// If return type is void. must put a simple POP.
	if funcDesc.FuncReturnSymbolDesc.returnType.TP == VoidVariableType {
		classAst.writeOutput("POP CONSTANT 0")
	}
}

func (classAst *ClassAst) generateArrayIndexCode(method *ClassFuncOrMethodAst, expr *ExpressionAst) {
	arrayVarName := expr.LeftExpr.(*ExpressionTerm)
	classAst.generateExpressionTermCode(method, arrayVarName)
	classAst.generateExpressionCode(method, expr.RightExpr.(*ExpressionAst))
	classAst.writeOutput("ADD")
	classAst.writeOutput("POP POINTER 1")
	classAst.writeOutput("PUSH THAT 0")
}

func (classAst *ClassAst) generateVarNameCode(method *ClassFuncOrMethodAst, varName string) {
	varSymbol, _ := symbolTable.lookUpVarInFunc(classAst.className, method.FuncName, varName)
	switch varSymbol.symbolType {
	case ClassStaticVariableSymbolType:
		classAst.writeOutput(fmt.Sprintf("PUSH STATIC %d", varSymbol.index))
	case ClassVariableSymbolType:
		// If this variable name is a class variable. then it must be a variable of current class.
		// Then we can get the variable from the first parameter of current method.
		// What if this variable is a class object, we must get
		classAst.writeOutput("PUSH ARGUMENT 0")
		classAst.writeOutput("POP POINTER 0")
		classAst.writeOutput("PUSH THIS %d")
	case FuncParamType:
		classAst.writeOutput(fmt.Sprintf("PUSH ARGUMENT %d", varSymbol.index))
	case FuncVariableType:
		classAst.writeOutput(fmt.Sprintf("PUSH LOCAL %d", varSymbol.index))
	}
}

func (classAst *ClassAst) generateConstantStringCode(str string) {
	classAst.writeOutput(fmt.Sprintf("PUSH %d", len(str)))
	classAst.writeOutput("CALL String.new 1")
	for _, character := range str {
		classAst.writeOutput(fmt.Sprintf("PUSH CONSTANT %d", character))
		classAst.writeOutput("CALL String.appendChar 1")
	}
}

func (classAst *ClassAst) generateOpCode(method *ClassFuncOrMethodAst, op *OpAst) {
	switch op.Op {
	case AddOpTP:
		classAst.writeOutput("ADD")
	case MinusOpTP:
		classAst.writeOutput("SUB")
	case MultipleOpTP:
		// If we use a * op, replace it with Math.multiple call.
		classAst.writeOutput("CALL Math.multiple 2")
	case DivideOpTP:
		classAst.writeOutput("CALL Math.divide 2")
	case AndOpTP:
		classAst.writeOutput("AND")
	case OrOpTP:
		classAst.writeOutput("OR")
	case LessOpTP:
		classAst.writeOutput("LT")
	case GreaterOpTP:
		classAst.writeOutput("GT")
	case EqualOpTp:
		classAst.writeOutput("EQ")
	case ArrayIndexOpTP:
		classAst.writeOutput("POP POINTER 1")
		classAst.writeOutput("PUSH THAT 1")
	}
}

func (classAst *ClassAst) generateUnaryOpCode(method *ClassFuncOrMethodAst, op *OpAst) {
	if op == nil {
		return
	}
	switch op.Op {
	case NegationOpTP:
		classAst.writeOutput("NEG")
	case BooleanNegationOpTP:
		classAst.writeOutput("NOT")
	}
}

func (classAst *ClassAst) writeOutput(output string) {
	output += "\n"
	for i := 0; i < len(output); {
		l, err := classAst.writer.Write([]byte(output[i:]))
		if err != nil {
			panic(err)
		}
		i += l
	}
}

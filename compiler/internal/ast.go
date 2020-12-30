package internal

import (
	"os"
)

// In this file, we defined all ast of hack programming languages according to hack programming language grammar.
// According to the grammar, each hack language file xxx.hack starts with a class definition, yes, there is no package
// declaration and dependency declaration.

type ClassAst struct {
	writer            *os.File
	path              string
	className         string
	classVariables    []*ClassVariableAst
	classFuncOrMethod []*ClassFuncOrMethodAst

	classSymbolTable *ClassSymbolTable // We set symbolTable reference here.
}

type ClassVariableAst struct {
	VariableName string
	FieldTP      FieldType
	VariableType VariableType

	symbolDesc *SymbolDesc // We set symbolTable reference here.
}

type FieldType int

const (
	ObjectFieldType FieldType = iota
	ClassFieldType
)

type VariableType struct {
	TP   VarType
	Name string
}

func (t VariableType) String() string {
	switch t.TP {
	case VoidVariableType:
		return "void"
	case IntVariableType:
		return "int"
	case CharVariableType:
		return "char"
	case BooleanVariableType:
		return "boolean"
	case ClassVariableType:
		return "class: " + t.Name
	}
	return ""
}

type VarType int

const (
	VoidVariableType VarType = iota // This only be used for method.
	IntVariableType
	CharVariableType
	BooleanVariableType
	ClassVariableType
)

type ClassFuncOrMethodAst struct {
	FuncTP   FuncType
	FuncName string
	ReturnTP VariableType
	Params   []*FuncParamAst
	FuncBody []*StatementAst

	funcSymbol *FuncSymbolTable // We set symbolTable reference here.
}

type FuncType int

const (
	ClassConstructorType FuncType = iota
	ClassMethodType
	ClassFuncType
)

type FuncParamAst struct {
	ParamName string
	ParamTP   VariableType

	symbolDesc *SymbolDesc // we put symbolDesc here.
}

type StatementAst struct {
	StatementTP StatementType
	Statement   interface{}
}

type StatementType int

const (
	VariableDeclareStatementTP StatementType = iota
	LetStatementTP
	IfStatementTP
	WhileStatementTP
	DoStatementTP
	ReturnStatementTP
)

type LetStatementAst struct {
	LetVariable *VariableAst
	Value       *ExpressionAst
}

type VariableAst struct {
	VarName string
	// If this variable is a array reference, can allow a array index expression to
	// locate the array element.
	ArrayIndex *ExpressionAst

	symbolDesc *SymbolDesc // We set symbolTable reference here.
}

type ExpressionAst struct {
	// Can be ExpressionAst or ExpressionTerm
	LeftExpr interface{}
	Op       *OpAst
	// Can be ExpressionAst or ExpressionTerm
	RightExpr interface{}
	TP        *VariableType
}

type ExpressionTerm struct {
	UnaryOp *OpAst
	Type    ExpressionTermType
	Value   interface{}
	TP      *VariableType
}

type ExpressionTermType int

const (
	// For constant value, the value in expression term is the value, for example:
	// * For 5, value is 5
	// * For "hello", value is "hello"
	IntegerConstantTermType ExpressionTermType = iota
	CharacterConstantTermType
	StringConstantTermType
	KeyWordConstantTrueTermType
	KeyWordConstantFalseTermType
	KeyWordConstantNullTermType
	KeyWordConstantThisTermType
	// For varName, value in expressionTerm is *VariableAst
	// In jack, it doesn't support varName.field to retrieve object field.
	VarNameExpressionTermType
	// For ArrayIndexExpressionTerm, value is *ExpressionAst
	// where left is ExpressionTerm, and right is *ExpressionAst
	ArrayIndexExpressionTermType
	// For subRoutineCallExpression, value is *CallAst
	SubRoutineCallTermType
	// For (subExpression), value is *ExpressionAst
	SubExpressionTermType
	// For unaryTermExpression like -expr, the value is *ExpressionAst
	UnaryTermExpressionTermType
)

type OpAst struct {
	OpTP     OpType
	Op       OpCode
	priority int
	Name     string
}

var (
	AddOpAst        = OpAst{OpTP: BinaryOPTP, Op: AddOpTP, priority: 3, Name: "+"}
	MinusOpAst      = OpAst{OpTP: BinaryOPTP, Op: MinusOpTP, priority: 3, Name: "-"}
	MultipleOpAst   = OpAst{OpTP: BinaryOPTP, Op: MultipleOpTP, priority: 4, Name: "*"}
	DivideOpAst     = OpAst{OpTP: BinaryOPTP, Op: DivideOpTP, priority: 4, Name: "/"}
	AndOpAst        = OpAst{OpTP: BinaryOPTP, Op: AndOpTP, priority: 1, Name: "&"}
	OrOpAst         = OpAst{OpTP: BinaryOPTP, Op: OrOpTP, priority: 1, Name: "|"}
	LessOpAst       = OpAst{OpTP: BinaryOPTP, Op: LessOpTP, priority: 2, Name: "<"}
	LessEqualOpAst  = OpAst{OpTP: BinaryOPTP, Op: LessEqualOpTP, priority: 2, Name: "<="}
	GreatOpAst      = OpAst{OpTP: BinaryOPTP, Op: GreaterOpTP, priority: 2, Name: ">"}
	GreatEqualOpAst = OpAst{OpTP: BinaryOPTP, Op: GreaterEqualOpTP, priority: 2, Name: ">="}
	EqualOpAst      = OpAst{OpTP: BinaryOPTP, Op: EqualOpTp, priority: 2, Name: "="}
	ArrayIndexOpAst = OpAst{OpTP: BinaryOPTP, Op: ArrayIndexOpTP, priority: 5, Name: "[]"}
)

func (op OpAst) String() string {
	switch op.Op {
	case AddOpTP:
		return "+"
	case MinusOpTP:
		return "-"
	case MultipleOpTP:
		return "*"
	case DivideOpTP:
		return "/"
	case AndOpTP:
		return "&"
	case OrOpTP:
		return "|"
	case LessOpTP:
		return "<"
	case LessEqualOpTP:
		return "<="
	case GreaterOpTP:
		return ">"
	case GreaterEqualOpTP:
		return ">="
	case EqualOpTp:
		return "="
	case ArrayIndexOpTP:
		return "[]"
	}
	return ""
}

type OpType int

const (
	UnaryOPTP OpType = iota
	BinaryOPTP
)

type OpCode int

const (
	AddOpTP OpCode = iota
	MinusOpTP
	MultipleOpTP
	DivideOpTP
	AndOpTP
	OrOpTP
	LessOpTP
	LessEqualOpTP
	GreaterOpTP
	GreaterEqualOpTP
	EqualOpTp
	ArrayIndexOpTP

	// Unary Op
	NegationOpTP
	BooleanNegationOpTP
)

type IfStatementAst struct {
	Condition        *ExpressionAst
	IfTrueStatements []*StatementAst
	ElseStatements   []*StatementAst
}

type WhileStatementAst struct {
	Condition  *ExpressionAst
	Statements []*StatementAst
}

type DoStatementAst struct {
	Call *CallAst
}

type CallAst struct {
	// We allow call like: Foo.m1(), where Foo is a class, or varName.
	// Also if just call m1(), then m1 is method of current Class.
	FuncProvider string
	FuncName     string
	Params       []*ExpressionAst

	FuncSymbolTable *FuncSymbolTable // We set symbolTable reference here.
}

type ReturnStatementAst struct {
	Return []*ExpressionAst
}

type VarDeclareAst struct {
	VarName string
	VarType VariableType

	symbolDesc *SymbolDesc // We set symbolTable reference here.
}

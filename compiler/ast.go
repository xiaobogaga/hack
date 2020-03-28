package compiler

import "io"

// In this file, we defined all ast of hack programming languages according to hack programming language grammar.
// According to the grammar, each hack language file xxx.hack starts with a class definition, yes, there is no package
// declaration and dependency declaration.

type ClassAst struct {
	writer            io.Writer
	className         string
	classVariables    []*ClassVariableAst
	classFuncOrMethod []*ClassFuncOrMethodAst
}

type ClassVariableAst struct {
	VariableNames []string
	FieldTP       FieldType
	VariableType  VariableType
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

type VarType int

const (
	IntVariableType VarType = iota
	CharVariableType
	BooleanVariableType
	ClassVariableType
	// Only used on type checking.
	ArrayType
	StringType
)

type ClassFuncOrMethodAst struct {
	FuncTP   FuncType
	FuncName string
	ReturnTP ReturnType
	Params   []*FuncParamAst
	FuncBody []*StatementAst
}

type FuncType int

const (
	ClassConstructorType FuncType = iota
	ClassMethodType
	ClassFuncType
)

type ReturnType struct {
	TP ReturnTP0
	// Only used for className type
	Name string
}

type ReturnTP0 int

const (
	VoidReturnType ReturnTP0 = iota
	IntReturnType
	CharReturnType
	BooleanReturnType
	ClassReturnType
)

type FuncParamAst struct {
	ParamName string
	ParamTP   VariableType
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
	GreaterOpTP
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
}

type ReturnStatementAst struct {
	Return []*ExpressionAst
}

type VarDeclareAst struct {
	VarNames []string
	VarType  VariableType
}

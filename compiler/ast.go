package compiler

// In this file, we defined all ast of hack programming languages according to hack programming language grammar.
// According to the grammar, each hack language file xxx.hack starts with a class definition, yes, there is no package
// declaration and dependency declaration.

type ClassAst struct {
	className         string
	classVariables    []*ClassVariableAst
	classFuncOrMethod []*ClassFuncOrMethodAst
}

type ClassVariableAst struct {
	VariableNames []string
	FieldTP       FieldType
	VariableType  VariableType
	// Only used when variableType is ClassType
	ClassName string
}

type FieldType int

const (
	ObjectFieldType FieldType = iota
	ClassFieldType
)

type VariableType int

const (
	IntVariableType VariableType = iota
	CharVariableType
	BooleanVariableType
	ClassVariableType
)

type ClassFuncOrMethodAst struct {
	FuncTP          FuncType
	FuncName        string
	ReturnTP        ReturnType
	ReturnClassName string
	Params          []*FuncParamAst
	FuncBody        []*StatementAst
}

type FuncType int

const (
	ClassConstructorType FuncType = iota
	ClassMethodType
	ClassFuncType
)

type ReturnType int

const (
	VoidReturnType ReturnType = iota
	IntReturnType
	CharReturnType
	BooleanReturnType
	ClassReturnType
)

type FuncParamAst struct {
	ParamName      string
	ParamTP        ParamType
	ParamClassName string
}

type ParamType int

const (
	IntParamType ParamType = iota
	CharParamType
	BooleanParamType
	ClassParamType
)

type StatementAst struct {
	StatementTP StatementType
	Statement   interface{}
}

type StatementType int

const (
	VariableDeclareTP StatementType = iota
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
	LeftExpr  *ExpressionTerm
	Op        *OpAst
	RightExpr *ExpressionTerm
}

type ExpressionTerm struct {
	UnaryOp *OpAst
	Type    ExpressionTermType
	Value   interface{}
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
	// For varName, value in expressionTerm is VariableAst
	VarNameExpressionTermType
	ArrayIndexExpressionTermType
	// For subRoutineCallExpression, value is CallAst
	SubRoutineCallTermType
	// For (subExpression), value is ExpressionAst
	SubExpressionTermType
	UnaryTermExpressionTermType
)

type OpAst struct {
	OpTP OpType
	Op   OpCode
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
	VarNames         []string
	VarType          VariableType
	VarTypeClassName string
}

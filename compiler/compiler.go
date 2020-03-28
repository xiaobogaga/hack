package compiler

import "flag"

var (
	path = flag.String("path", ".", "the path of source file")
)

func compile() {
	parser := &Parser{}
	classAsts, err := parser.Parse(*path)
	if err != nil {
		panic(err)
	}
	err = buildSymbolTables(classAsts)
	if err != nil {
		panic(err)
	}
	// Todo: Semantic analysis
	err = SymbolExistenceChecker(classAsts)
	if err != nil {
		panic(err)
	}
	err = typeChecker(classAsts)
	if err != nil {
		panic(err)
	}
	err = methodReturnAnalysisOnClasses(classAsts)
	if err != nil {
		panic(err)
	}
	// Todo: generate code
	codeGenerator := CodeGenerate{}
	err = codeGenerator.generate(classAsts)
	if err != nil {
		panic(err)
	}
}

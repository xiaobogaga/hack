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
	err = symbolTable.buildSymbolTables(classAsts)
	if err != nil {
		panic(err)
	}
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
	err = generateCodes(classAsts)
	if err != nil {
		panic(err)
	}
}

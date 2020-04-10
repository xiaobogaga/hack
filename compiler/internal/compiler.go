package internal

func Compile(path string) error {
	parser := &Parser{}
	classAsts, err := parser.Parse(path)
	if err != nil {
		return err
	}
	err = symbolTable.buildSymbolTables(classAsts)
	if err != nil {
		return err
	}
	err = SymbolExistenceChecker(classAsts)
	if err != nil {
		return err
	}
	err = typeChecker(classAsts)
	if err != nil {
		return err
	}
	err = methodReturnAnalysisOnClasses(classAsts)
	if err != nil {
		return err
	}
	err = generateCodes(classAsts)
	if err != nil {
		return err
	}
	return nil
}

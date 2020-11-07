package internal

func Compile(path string, skipCheck bool) error {
	parser := &Parser{}
	println("compiler: start parser at path: " + path)
	classAsts, err := parser.Parse(path)
	if err != nil {
		return err
	}
	symbolTable.initStandardLibrary()
	println("compiler: start building symbol table")
	err = symbolTable.buildSymbolTables(classAsts)
	if err != nil {
		return err
	}
	if !skipCheck {
		println("compiler: start existence checker")
		err = SymbolExistenceChecker(classAsts)
		if err != nil {
			return err
		}
		println("compiler: start type checker")
		err = typeChecker(classAsts)
		if err != nil {
			return err
		}
		println("compiler: start return analysis")
		err = methodReturnAnalysisOnClasses(classAsts)
		if err != nil {
			return err
		}
	}
	println("compiler: start generate codes")
	err = generateCodes(classAsts)
	if err != nil {
		return err
	}
	return nil
}

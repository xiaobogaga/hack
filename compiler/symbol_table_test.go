package compiler

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestClassSymbolTable_buildClassVariables(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testDatas := []struct {
		data      string
		expectErr bool
	}{
		{
			data: `
			class Test {
				static int a;
				field char c;
				static char b;
				field Human m;
			}
			`,
			expectErr: false,
		},
		{
			data: `
			class Test {
				static int a;
				static char a;
			}
					`,
			expectErr: true,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, testData := range testDatas {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData.data)))
		assert.Nil(t, err)
		assert.NotNil(t, tokens)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, classAst)
		symbolTable := &ClassSymbolTable{
			ClassName:            "Test",
			VariablesSymbolTable: map[string]*SymbolDesc{},
			FuncSymbolTable:      map[string]*FuncSymbolTable{},
		}
		err = symbolTable.buildClassVariables(classAst)
		if testData.expectErr {
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestClassSymbolTable_buildClassMethods(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testDatas := []struct {
		data      string
		expectErr bool
	}{
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					return;
				}
				method void print() {
					return;
				}
			}
			`,
			expectErr: false,
		},
		{
			data: `
			class Test {
				constructor Test Test() {

				}
				method void Test() {

				}
			}
					`,
			expectErr: true,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, testData := range testDatas {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData.data)))
		assert.Nil(t, err)
		assert.NotNil(t, tokens)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, classAst)
		symbolTable := &ClassSymbolTable{
			ClassName:            "Test",
			VariablesSymbolTable: map[string]*SymbolDesc{},
			FuncSymbolTable:      map[string]*FuncSymbolTable{},
		}
		err = symbolTable.buildClassMethods(classAst)
		if testData.expectErr {
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestClassSymbolTable_buildFuncParams(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testDatas := []struct {
		data      string
		expectErr bool
	}{
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					return;
				}
			}
			`,
			expectErr: false,
		},
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean a) {
				}
			}
					`,
			expectErr: true,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, testData := range testDatas {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData.data)))
		assert.Nil(t, err)
		assert.NotNil(t, tokens)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, classAst)
		symbolTable := &ClassSymbolTable{
			ClassName:            "Test",
			VariablesSymbolTable: map[string]*SymbolDesc{},
			FuncSymbolTable:      map[string]*FuncSymbolTable{},
		}
		funcSymbolTable := &FuncSymbolTable{
			FuncSymbolDesc: &SymbolDesc{
				name: "Test",
			},
		}
		err = symbolTable.buildFuncParams(classAst.classFuncOrMethod[0], funcSymbolTable)
		if testData.expectErr {
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestClassSymbolTable_buildFuncLocalVariableDesc(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testDatas := []struct {
		data      string
		expectErr bool
	}{
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					return;
				}
				method void print() {
				}
			}
			`,
			expectErr: false,
		},
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					var int i, a;
					var char a;
					return;
				}
			}
					`,
			expectErr: true,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, testData := range testDatas {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData.data)))
		assert.Nil(t, err)
		assert.NotNil(t, tokens)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, classAst)
		symbolTable := &ClassSymbolTable{
			VariablesSymbolTable: map[string]*SymbolDesc{},
			FuncSymbolTable:      map[string]*FuncSymbolTable{},
		}
		funcSymbolTable := &FuncSymbolTable{
			FuncSymbolDesc: &SymbolDesc{
				name: "Test",
			},
		}
		err = symbolTable.buildFuncLocalVariableDesc(classAst.classFuncOrMethod[0], funcSymbolTable)
		if testData.expectErr {
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
		}
	}
}

func TestBuildClassSymbolTable(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testDatas := []struct {
		data      string
		expectErr bool
	}{
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					return;
				}
				static int a;
				method void print() {
				}
				field char b;
			}
			`,
			expectErr: false,
		},
		{
			data: `
			class Test {
				constructor Test Test(int a, char b, boolean c, Human h) {
					var int i, a;
					var char a;
					return;
				}
			}
					`,
			expectErr: true,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, testData := range testDatas {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData.data)))
		assert.Nil(t, err)
		assert.NotNil(t, tokens)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, classAst)
		_, err = buildClassSymbolTable(classAst)
		if testData.expectErr {
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
		}
		// print(classSymbolTable)
	}
}

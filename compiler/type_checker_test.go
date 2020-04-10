package compiler

import (
	"bytes"
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func initTestData(t *testing.T) {
	parser := &Parser{}
	tokenizer := &Tokenizer{}
	data := []string{
		`
			class Test1 {
				static int a;
				field char c;
				constructor Test1 new(char c1) {
					let c = c1;
					return this;
				}
				method void set(char c1) {
					let c = c1;
					return;
				}
				method char get() {
					return c;
				}
				function void print() {
					return;
				}
			}
		`,
		`
			class Test2 {
				static int b;
				field boolean c;
				constructor Test2 new(boolean c1, Test1 t1) {
					let c = c1;
					let t = t1;
					return this;
				}
				field Test1 t;
				method void set(boolean c1, char c2) {
					let c = c1;
					do t.set(c2);
					return;
				}
				method char getTest1C() {
					return t.get();
				}
			}
		`,
	}
	var testClassAst []*ClassAst
	for _, d := range data {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(d)))
		assert.Nil(t, err, d)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err, d)
		testClassAst = append(testClassAst, classAst)
	}
	err := symbolTable.buildSymbolTables(testClassAst)
	assert.Nil(t, err)
}

func TestTypeChecker_classVariableExistenceCheck(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testData := []struct {
		fileContent string
		expectErr   bool
	}{
		{
			fileContent:
			`
				class Test {
					field int a;
					field Test1 t1;
					static Test2 t2;
				}
				`,
			expectErr: false,
		},
		{
			fileContent:
			`
				class Test {
					field int a;
					field Test1 t1;
					field Test3 t3;
				}
				`,
			expectErr: true,
		},
	}
	parser := &Parser{}
	tokenizer := &Tokenizer{}
	for _, d := range testData {
		initTestData(t)
		parser.reset()
		tokenizer.Reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(d.fileContent)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		err = symbolTable.buildSymbolTables([]*ClassAst{classAst})
		assert.Nil(t, err)
		err = classAst.checkClassVariablesExistence()
		if d.expectErr {
			assert.NotNil(t, err, d.fileContent)
			fmt.Printf("%+v\n", err)
		} else {
			assert.Nil(t, err, d.fileContent)
		}
		// Reset symbol table.
		symbolTable = map[string]*ClassSymbolTable{}
	}
}

func TestTypeChecker_funcVariablesExistenceCheck(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	testData := []struct {
		fileContent string
		expectErr   bool
	}{
		{
			fileContent:
			`
				class Test {
					field int a1;
					static char c1;
					static Test1 c11;
					constructor Test new(int a, Test b, Test1 c) {
						var int d1;
						var Test d;
						do c.set(c[1]);
						do Test1.print();
						do c.print();
						let a = a + 1;
						let b = a * b[1] + c.get();
					}

					method void set() {
						return;
					}
					
					function void print() {
						let c1 = c1 + 1;
						// let a1 = a1 + 1;
						do c11.set(1);
						// do c11.print();
						// do set();
						return c11.set(1);
					}
				}
				`,
			expectErr: false,
		},
	}
	parser := &Parser{}
	tokenizer := &Tokenizer{}
	for _, d := range testData {
		initTestData(t)
		parser.reset()
		tokenizer.Reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(d.fileContent)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		classAst, err := parser.ParseClassDeclaration()
		assert.Nil(t, err)
		err = symbolTable.buildSymbolTables([]*ClassAst{classAst})
		assert.Nil(t, err)
		err = classAst.checkFuncVariableExistence(classAst.classFuncOrMethod[2])
		if d.expectErr {
			assert.NotNil(t, err, d.fileContent)
			fmt.Printf("%+v\n", err)
		} else {
			assert.Nil(t, err, d.fileContent)
		}
		// Reset symbol table.
		symbolTable = map[string]*ClassSymbolTable{}
	}
}

func TestTypeChecker_typeCheckStatements(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	data := `
				class Test {
					field int a;
					field char b;
					static boolean c;
					field Test1 t;
					static Test1 t1;
					static Test1 t2;
					static Test2 t3;
					constructor Test Test(int a, char b, boolean c) {
						// let a = ('b' - 1 / 2) + 1 > 0;
						let b = b + -1;
						let c = ~(~((1 + 2) > 0));
						let t = null;
						do t1.set(b);
						let t3 = t2 + t3;
						return null;
					}
					method Test print() {
						let a = a;
						return a + 1;
						// let a = 1;
					}
					function void T() {
						if (t1 = t2) {
							return;
						} else {
							if (c > 0) {
								let c = c + 1;
								return;
							}
							return;
						}
					}
				}
			`
	parser := &Parser{}
	tokenizer := &Tokenizer{}
	initTestData(t)
	parser.reset()
	tokenizer.Reset()
	tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data)))
	assert.Nil(t, err)
	parser.currentTokens = tokens
	classAst, err := parser.ParseClassDeclaration()
	assert.Nil(t, err)
	err = symbolTable.buildSymbolTables([]*ClassAst{classAst})
	assert.Nil(t, err)
	err = classAst.existenceCheck()
	assert.Nil(t, err)
	// err = classAst.methodReturnAnalysis()
	err = classAst.typeChecker0()
	fmt.Printf("%+v\n", err)
}

func TestTypeChecker_returnAnalysis(t *testing.T) {
	symbolTable = map[string]*ClassSymbolTable{}
	data := `
				class Test {
					field int a;
					field char b;
					static boolean c;
					constructor Test Test(int a, char b, boolean c) {
						let a = a;
						let b = 'c';
						let c = c;
						return this;
					}
					method void print() {
						let a = a;
						return;
						// let a = 1;
					}
					function void T() {
						if (c > 0) {
							return;
						} else {
							if (c > 0) {
								let c = c + 1;
								return;
							}
							return;
						}
					}
				}
			`
	parser := &Parser{}
	tokenizer := &Tokenizer{}
	// initTestData(t)
	parser.reset()
	tokenizer.Reset()
	tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data)))
	assert.Nil(t, err)
	parser.currentTokens = tokens
	classAst, err := parser.ParseClassDeclaration()
	assert.Nil(t, err)
	err = symbolTable.buildSymbolTables([]*ClassAst{classAst})
	assert.Nil(t, err)
	err = classAst.checkClassVariablesExistence()
	assert.Nil(t, err)
	err = classAst.methodReturnAnalysis()
	fmt.Printf("%+v\n", err)
}

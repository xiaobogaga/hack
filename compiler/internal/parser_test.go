package internal

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestParser_isJackFile(t *testing.T) {
	fileName := "xxx.jack"
	assert.True(t, isJackFile(fileName))
	fileName = "xxx.j1ack1"
	assert.False(t, isJackFile(fileName))
}

func TestParser_ParseExpression(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{Content: "a + b"},
		{Content: "a + b * c"},
		{Content: "a * b + c * d"},
		{Content: "a[1 + e * f] * g + b * c"},
		{Content: "a.b(d, e * i) + c * f + g[h * i]"},
		{Content: "a.b(c[1] + d.e(f)) + g[i * j.l(m)] + h"},
		{Content: "b(c, e) + f"},
		{Content: "(a + b + (c * (a + (b)))) * c + (a * (a + b))"},
		{Content: "a = 1 & c = 2"},
		{Content: "i = 1 * 1 + 1 | j = 2"},
		{Content: "i | j = 2 + 1 = 3 * 2 - 1 | 0 * 4 & h / 3 | 8 > 3 | 3 * 2 + 1 < 9"},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		// printTestTokens(tokens)
		assert.Nil(t, err)
		parser.currentTokens = tokens
		ast, err := parser.parseExpression()
		assert.Nil(t, err, data)
		// fmt.Printf("%+v\n", ast)
		printExprAst(ast)
		assert.NotNil(t, ast)
	}
}

func printExprAst(ast *ExpressionAst) {
	if ast == nil {
		return
	}
	buf := &bytes.Buffer{}
	encoder := json.NewEncoder(buf)
	encoder.SetEscapeHTML(false)
	encoder.SetIndent("", "\t\t")
	err := encoder.Encode(ast)
	if err != nil {
		panic(err)
	}
	println(buf.String())
	printExprAstAsXml(ast)
}

func printExprAstAsXml(ast *ExpressionAst) {
	if ast == nil {
		return
	}
	buf := &bytes.Buffer{}
	encoder := xml.NewEncoder(buf)
	encoder.Indent("", "\t\t")
	err := encoder.Encode(ast)
	if err != nil {
		panic(err)
	}
	println(buf.String())
}

func TestParser_ParseReturnStatement(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{Content: "return;"},
		{Content: "return 1;"},
		{Content: "return a;"},
		{Content: "return b.c(a);"},
		{Content: "return a[1];"},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		stm, err := parser.parseReturnStatement()
		assert.Nil(t, err)
		assert.NotNil(t, stm)
	}
}

func TestParser_ParseWhileStatement(t *testing.T) {
	testData := []struct {
		Content string
	}{
		// {Content: `while (a > 1) {}`},
		{Content: "while (a[1] > 1) { do b.c(); }"},
		{Content: "while (a.b(c) > g) { do e.h(i, j[1]); }"},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		stm, err := parser.parseWhileStatement()
		assert.Nil(t, err, data.Content)
		assert.NotNil(t, stm, data.Content)
	}
}

func TestParser_ParseIfStatement(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{
			Content: `
				if (a[1] = 0) {
					if (b = 1) {
						let c = c + 1;
					}
					do b.c();
				}
				`,
		},
		{
			Content: `
				if (a.c(1) + a[1] * g = 1) {
					let b = b + 1;
				} else {
					let b = b + 2;
				}
				`,
		},

		{
			Content: `
				if (a = 1) {
					let b = b + 1;
					if (a = 2) {
						let b = b + 2;
					} else {
						let b = b + 3;
					}
				} else {
					if (b = 1) {
						let c = c + 1;
					} else {
						let c = c + 2;
					}
				}
				`,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err, data.Content)
		parser.currentTokens = tokens
		stm, err := parser.parseIfStatement()
		assert.Nil(t, err, data.Content)
		assert.NotNil(t, stm, data.Content)
	}
}

func TestParser_ParseDoStatement(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{Content: "do a.b(1);"},
		{Content: "do a.b();"},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		stm, err := parser.parseDoStatement()
		assert.Nil(t, err)
		assert.NotNil(t, stm)
	}
}

func TestParser_ParseVarDeclareStatement(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{Content: `
					var int a;
					var Human my;
				`},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		stms, err := parser.parseVarDeclareStatement()
		assert.Nil(t, err)
		assert.NotNil(t, stms)
	}
}

func TestParser_parseStatements(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{Content: `
					var int a;
					do a.b(1);
					let a = a + 1;
					do c.b(2);
					if (a > 1) {
						do c.b();
					}
					while (a > 1) {
						do c.b();
					}
					return;
				`},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		stms, err := parser.parseStatements()
		assert.Nil(t, err)
		assert.NotNil(t, stms)
	}
}

func TestParser_ParseFuncOrMethodDeclaration(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{
			Content: `
				constructor void print() {
					do print("hello");
				}
				`,
		},
		{
			Content: `
				function char test(int a, char b, Human p, boolean c) {
					var int c;
					let c = a;
					if (c > 0) {
						do print(c);
					}
					return;
				}
					`,
		},
		{
			Content: `
				method boolean test1() {
					return;
				}
					`,
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		funcDeclare, err := parser.ParseFuncOrMethodDeclaration("test")
		assert.Nil(t, err)
		assert.NotNil(t, funcDeclare)
	}
}

func TestParser_ParseVariableDeclaration(t *testing.T) {
	testData := []struct {
		Content string
	}{
		{
			Content: "field int a, b;",
		},
		{
			Content: "static char b, c;",
		},
		{
			Content: "field int a;",
		},
		{
			Content: "field Human h;",
		},
	}
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	for _, data := range testData {
		tokenizer.Reset()
		parser.reset()
		tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(data.Content)))
		assert.Nil(t, err)
		parser.currentTokens = tokens
		varDeclare, err := parser.ParseVariableDeclaration()
		assert.Nil(t, err)
		assert.NotNil(t, varDeclare)
	}
}

func TestParser_ParseClassDeclaration(t *testing.T) {
	testData := `

				class Test {
					constructor Test Test(int b) {
						let c = 'a';
						let a = 10;
						let a = b;
					}
					static int a;
					field char c;
					method void print() {
						do print(a);
					}
					function void t() {
						do print(a);
					}
				}

				`
	tokenizer := &Tokenizer{}
	parser := &Parser{}
	tokenizer.Reset()
	parser.reset()
	tokens, err := tokenizer.Tokenize(bytes.NewReader([]byte(testData)))
	assert.Nil(t, err)
	parser.currentTokens = tokens
	classAst, err := parser.ParseClassDeclaration()
	assert.Nil(t, err)
	assert.NotNil(t, classAst)
}

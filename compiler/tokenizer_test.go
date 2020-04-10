package compiler

import (
	"bytes"
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestTokenizer_TrimSpace(t *testing.T) {
	testData := []struct {
		content     string
		expectedPos int
	}{
		{content: "   	hello", expectedPos: 4},
		{content: `     
					hello \nhihi`, expectedPos: 10},
	}
	tokenizer := &Tokenizer{}
	for _, testD := range testData {
		tokenizer.trimSpace([]byte(testD.content))
		assert.Equal(t, testD.expectedPos, testD.expectedPos)
	}
}

func TestTokenizer_hasRemainCharacters(t *testing.T) {
	tokenizer := &Tokenizer{}
	tokenizer.currentPos = 0
	assert.True(t, tokenizer.hasRemainCharacters([]byte("b")))
	tokenizer.currentPos = 1
	assert.False(t, tokenizer.hasRemainCharacters([]byte("b")))
}

func TestTokenizer_TokenSimpleSymbol(t *testing.T) {
	testData := []struct {
		symbol        []byte
		expectedToken *Token
	}{
		// Six is my favorite number.
		{symbol: []byte("{"), expectedToken: &Token{
			content: "{",
			endPos:  1,
			tp:      LeftBraceTP,
		}},
		{symbol: []byte("}"), expectedToken: &Token{
			content: "}",
			endPos:  1,
			tp:      RightBraceTP,
		}},
		{symbol: []byte("*"), expectedToken: &Token{
			content: "*",
			endPos:  1,
			tp:      MultiplyTP,
		}},
		{symbol: []byte(">"), expectedToken: &Token{
			content: ">",
			endPos:  1,
			tp:      GreaterTP,
		}},
		{symbol: []byte(";"), expectedToken: &Token{
			content: ";",
			endPos:  1,
			tp:      SemiColonTP,
		}},
		{symbol: []byte("&"), expectedToken: &Token{
			content: "&",
			endPos:  1,
			tp:      AndTP,
		}},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		token, err := tokenizer.tokenSimpleSymbol(data.symbol)
		assert.Nil(t, err)
		assert.Equal(t, data.expectedToken, token)
		assert.Equal(t, 1, tokenizer.currentPos)
	}
}

func TestTokenizer_TokenCommentOrDivide(t *testing.T) {
	testData := []struct {
		line          []byte
		currentPos    int
		expectedToken *Token
	}{
		{line: []byte("/"), currentPos: 1, expectedToken: &Token{
			content: "/",
			tp:      DivideTP,
			endPos:  1,
		}},
		{line: []byte("/*"), currentPos: 2, expectedToken: &Token{
			content: "/*",
			tp:      MultipleLineOpenCommentTP,
			endPos:  2,
		}},
		{line: []byte("//"), currentPos: 2, expectedToken: &Token{
			content: "//",
			tp:      SingleLineCommentTP,
			endPos:  2,
		}},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		token, err := tokenizer.tokenCommentOrDivide(data.line)
		assert.Nil(t, err)
		assert.Equal(t, data.expectedToken, token)
		assert.Equal(t, data.currentPos, tokenizer.currentPos)
	}
}

func TestTokenizer_TokenCharacter(t *testing.T) {
	testData := []struct {
		line          []byte
		currentPos    int
		expectedToken *Token
	}{
		{line: []byte(`'a'`), currentPos: 3, expectedToken: &Token{
			content:  "a",
			tp:       CharacterTP,
			startPos: 1,
			endPos:   2,
		}},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		token, err := tokenizer.tokenCharacter(data.line)
		assert.Nil(t, err)
		assert.Equal(t, data.expectedToken, token)
		assert.Equal(t, data.currentPos, tokenizer.currentPos)
	}
}

func TestTokenizer_TokenString(t *testing.T) {
	testData := []struct {
		line               []byte
		expectedToken      *Token
		expectedCurrentPos int
	}{
		{
			line:               []byte(`" hello"`),
			expectedCurrentPos: 8,
			expectedToken: &Token{
				content:  " hello",
				endPos:   7,
				startPos: 1,
				tp:       StringTP,
			}},
		{
			line:               []byte(`"hello`),
			expectedCurrentPos: 6,
			expectedToken:      nil},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		ret, err := tokenizer.tokenString(data.line)
		if data.expectedToken == nil {
			assert.NotNil(t, err)
			assert.Nil(t, ret)
		} else {
			assert.Nil(t, err)
			assert.Equal(t, data.expectedToken, ret)
		}
		assert.Equal(t, data.expectedCurrentPos, tokenizer.currentPos)
	}
}

func TestTokenizer_TokenNumber(t *testing.T) {
	testData := []struct {
		line               []byte
		expectedToken      *Token
		expectedCurrentPos int
	}{
		{line: []byte(`12345`), expectedCurrentPos: 5, expectedToken: &Token{
			content: "12345",
			endPos:  5,
			tp:      IntegerTP,
		}},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		ret, err := tokenizer.tokenNumber(data.line)
		assert.Nil(t, err)
		assert.Equal(t, data.expectedToken, ret)
		assert.Equal(t, data.expectedCurrentPos, tokenizer.currentPos)
	}
}

func TestTokenizer_TokenKeyWord(t *testing.T) {
	testData := []struct {
		keyWord       []byte
		expectedToken *Token
	}{
		{keyWord: []byte("class"), expectedToken: &Token{
			content: "class",
			tp:      ClassTP,
		}},
		{keyWord: []byte("void"), expectedToken: &Token{
			content: "void",
			tp:      VoidTP,
		}},
		{keyWord: []byte("var"), expectedToken: &Token{
			content: "var",
			tp:      VarTP,
		}},
		{keyWord: []byte("ccc"),},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		token, isKeyWord := tokenizer.tryTransformToKeyWord(data.keyWord)
		if data.expectedToken == nil {
			assert.Nil(t, token)
			assert.False(t, isKeyWord)
		} else {
			assert.True(t, isKeyWord)
			assert.Equal(t, data.expectedToken, token)
		}
	}
}

func TestTokenizer_TokenIdentifier(t *testing.T) {
	testData := []struct {
		identifier    []byte
		expectedToken *Token
	}{
		{identifier: []byte("name1"), expectedToken: &Token{
			content: "name1",
			tp:      IdentifierTP,
		}},
		{identifier: []byte("_size12"), expectedToken: &Token{
			content: "_size12",
			tp:      IdentifierTP,
		}},
		{identifier: []byte("size_1"), expectedToken: &Token{
			content: "size_1",
			tp:      IdentifierTP,
		}},
		{identifier: []byte("1_name"),},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		token, err := tokenizer.tryTransformToIdentifier(data.identifier)
		if data.expectedToken == nil {
			assert.Nil(t, token)
			assert.NotNil(t, err)
		} else {
			assert.Nil(t, err)
			assert.Equal(t, data.expectedToken, token)
		}
	}
}

func TestTokenizer_TokenKeyWordOrIdentifier(t *testing.T) {
	testData := []struct {
		line          []byte
		expectedToken *Token
	}{
		// Six is my favorite number.
		{line: []byte("class"), expectedToken: &Token{
			content:  "class",
			tp:       ClassTP,
			startPos: 0,
			endPos:   5,
		}},
		{line: []byte("void"), expectedToken: &Token{
			content:  "void",
			tp:       VoidTP,
			startPos: 0,
			endPos:   4,
		}},
		{line: []byte("var"), expectedToken: &Token{
			content:  "var",
			tp:       VarTP,
			startPos: 0,
			endPos:   3,
		}},
		{line: []byte("name1"), expectedToken: &Token{
			content:  "name1",
			tp:       IdentifierTP,
			startPos: 0,
			endPos:   5,
		}},
		{line: []byte("_size12"), expectedToken: &Token{
			content:  "_size12",
			tp:       IdentifierTP,
			startPos: 0,
			endPos:   7,
		}},
		{line: []byte("size_1"), expectedToken: &Token{
			content:  "size_1",
			tp:       IdentifierTP,
			startPos: 0,
			endPos:   6,
		}},
		{line: []byte("1_name"),},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		ret, err := tokenizer.toKeywordOrIdentifier(data.line)
		if data.expectedToken == nil {
			assert.NotNil(t, err)
			assert.Nil(t, ret)
		} else {
			assert.Nil(t, err)
			assert.Equal(t, data.expectedToken, ret)
		}
	}
}

func TestTokenizer_ParseLine(t *testing.T) {
	testData := []struct {
		line  string
		match bool
	}{
		{
			line:  " // hello world /* */",
			match: true,
		},
		{
			line:  "/* hello 1*/ /* /*",
			match: false,
		},
		{
			line:  "/* hello /* /*",
			match: false,
		},
		{
			/* /* hello */
			line:  "/* /* hello */",
			match: true,
		},
		{
			/* */ /* /* /* */
			line:  "/* */ /* /* /* */",
			match: true,
		},
		{
			// /* */ /* /* /* */ /*
			line:  "/* */ /* /* /* */ /*",
			match: false,
		},
	}
	tokenizer := &Tokenizer{}
	for _, data := range testData {
		tokenizer.Reset()
		err, match := tokenizer.parseLine([]byte(data.line))
		assert.Nil(t, err)
		assert.Equal(t, data.match, match)
	}
}

func TestTokenizer_Tokenize(t *testing.T) {
	content := []byte(`
// A simple hello world jack program

/*  
 ** comment |
	a main func
/* /*
*/

class Main {

	function void main() {
        var int a;
        var char c;
        var String d;
        let a = 10;
		let c = 'b';
        let b = c + a;
		do Output.printString("Hello world");
		do Output.println();
		return;
	}

}
`)
	tokenizer := &Tokenizer{}
	_, err := tokenizer.Tokenize(bytes.NewReader(content))
	assert.Nil(t, err)
	// printTestTokens(tokens)
}

func printTestTokens(tokens []*Token) {
	for _, t := range tokens {
		fmt.Printf("%+v\n", t)
	}
}

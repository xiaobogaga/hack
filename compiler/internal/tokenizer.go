package internal

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"nands/util"
	"unicode"
)

// A simple Tokenizer for hack.

// Hack language has those elements:
// * KeyWord: class, constructor, function, method, field, static, var, int, char, boolean, void, true.
// 			false, null, this, let, do, if, else, while, return.
// * Symbol: {, }, (, ), [, ], ., ,, ;, +, -, *, /, &, |, <, >, =, -.
// * Constant: integer, string ("xxx")
// * Identifier: letters, digits, underscore, not starting with a digit.
// * Comment: /**/, //.

type TokenType int

const (
	ClassTP                    TokenType = iota // class
	ConstructorTP                               // constructor
	FunctionTP                                  // function
	MethodTP                                    // method
	FieldTP                                     // field
	StaticTP                                    // static
	VarTP                                       // var
	IntTP                                       // int
	CharTP                                      // char
	BooleanTP                                   // boolean
	VoidTP                                      // void
	TrueTP                                      // true
	FalseTP                                     // false
	NullTP                                      // null
	ThisTP                                      // this
	LetTP                                       // let
	DoTp                                        // do
	IfTP                                        // if
	ElseTP                                      // else
	WhileTP                                     // while
	ReturnTP                                    // return
	LeftBraceTP                                 // {
	RightBraceTP                                // }
	LeftParentThesesTP                          // (
	RightParentThesesTP                         // )
	LeftSquareBracketTP                         // [
	RightSquareBracketTP                        // ]
	DotTP                                       // .
	CommaTP                                     // ,
	SemiColonTP                                 // ;
	AddTP                                       // +
	MinusTP                                     // -
	MultiplyTP                                  // *
	DivideTP                                    // /
	AndTP                                       // &
	OrTP                                        // |
	GreaterTP                                   // >
	LessTP                                      // <
	EqualTP                                     // =
	BooleanNegativeTP                           // ~
	IntegerTP                                   // 1010
	StringTP                                    // "xxx"
	IdentifierTP                                // varA
	MultipleLineOpenCommentTP                   // /*
	MultipleLineCloseCommentTP                  // */
	SingleLineCommentTP                         // //
	CharacterTP                                 // 'a'
)

// keyWordTokenTPMap is the mapping from identifier to the corresponding TokenTP.
var keyWordTokenTPMap = map[string]TokenType{
	"class":       ClassTP,
	"constructor": ConstructorTP,
	"function":    FunctionTP,
	"method":      MethodTP,
	"field":       FieldTP,
	"static":      StaticTP,
	"var":         VarTP,
	"int":         IntTP,
	"char":        CharTP,
	"boolean":     BooleanTP,
	"void":        VoidTP,
	"true":        TrueTP,
	"false":       FalseTP,
	"null":        NullTP,
	"this":        ThisTP,
	"let":         LetTP,
	"do":          DoTp,
	"if":          IfTP,
	"else":        ElseTP,
	"while":       WhileTP,
	"return":      ReturnTP,
}

// simpleSymbolTokenTPMap is the mapping from simple identifier to the corresponding TokenTP.
// There are some symbols which are very easy to distinguish, so we put those together.
var simpleSymbolTokenTPMap = map[string]TokenType{
	"{": LeftBraceTP,
	"}": RightBraceTP,
	"(": LeftParentThesesTP,
	")": RightParentThesesTP,
	"[": LeftSquareBracketTP,
	"]": RightSquareBracketTP,
	".": DotTP,
	",": CommaTP,
	";": SemiColonTP,
	"+": AddTP,
	"-": MinusTP,
	"*": MultiplyTP,
	"&": AndTP,
	"|": OrTP,
	">": GreaterTP,
	"<": LessTP,
	"=": EqualTP,
	"~": BooleanNegativeTP,
}

type Token struct {
	content  string
	line     int
	startPos int
	endPos   int
	tp       TokenType
}

func (t *Token) Value(tp TokenType) interface{} {
	switch tp {
	case IdentifierTP:
		return t.content
	}
	return nil
}

type Tokenizer struct {
	currentPos  int
	currentFile string
	currentLine int
	tokens      []*Token
}

// getNextToken returns the next token from line. where Token is the returned token.
func (tokenizer *Tokenizer) getNextToken(line []byte) (*Token, error) {
	tokenizer.trimSpace(line)
	if !tokenizer.hasRemainCharacters(line) {
		return nil, nil
	}
	switch line[tokenizer.currentPos] {
	case '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '&', '|', '>', '<', '=', '~':
		return tokenizer.tokenSimpleSymbol(line)
	case '/':
		return tokenizer.tokenCommentOrDivide(line)
	case '\'':
		return tokenizer.tokenCharacter(line)
	case '"':
		return tokenizer.tokenString(line)
	case '1', '2', '3', '4', '5', '6', '7', '8', '9', '0':
		return tokenizer.tokenNumber(line)
	default:
		return tokenizer.toKeywordOrIdentifier(line)
	}
}

// trimSpace will step forward through line and skip all continuous space, return the remaining line
// which must start with a non-space character.
func (tokenizer *Tokenizer) trimSpace(line []byte) {
	for tokenizer.currentPos < len(line) {
		l := line[tokenizer.currentPos]
		if unicode.IsSpace(rune(l)) {
			tokenizer.currentPos++
			continue
		}
		break
	}
	return
}

func (tokenizer *Tokenizer) hasRemainCharacters(line []byte) bool {
	return tokenizer.currentPos < len(line)
}

func (tokenizer *Tokenizer) tokenSimpleSymbol(line []byte) (*Token, error) {
	symbol := string(line[tokenizer.currentPos])
	token := &Token{
		content:  symbol,
		line:     tokenizer.currentLine,
		tp:       simpleSymbolTokenTPMap[symbol],
		startPos: tokenizer.currentPos,
		endPos:   tokenizer.currentPos + 1,
	}
	tokenizer.currentPos++
	return token, nil
}

func (tokenizer *Tokenizer) tokenCommentOrDivide(line []byte) (*Token, error) {
	// If / is not followed by * or /, then it's not a comment.
	if len(line[tokenizer.currentPos:]) == 1 || (line[tokenizer.currentPos+1] != '/' && line[tokenizer.currentPos+1] != '*') {
		return tokenizer.tokenDivide(line)
	}
	switch line[tokenizer.currentPos+1] {
	case '/':
		return tokenizer.tokenSingleLineComment(line)
	default:
		return tokenizer.tokenMultipleLineOpenComment(line)
	}
}

func (tokenizer *Tokenizer) tokenCharacter(line []byte) (*Token, error) {
	tokenizer.currentPos++
	if tokenizer.currentPos >= len(line) {
		return nil, tokenizer.makeError(string(line), tokenizer.currentLine, "incorrect character format")
	}
	tokenizer.currentPos++
	if tokenizer.currentPos >= len(line) || line[tokenizer.currentPos] != '\'' {
		return nil, tokenizer.makeError(string(line), tokenizer.currentLine, "incorrect character format")
	}
	tokenizer.currentPos++
	return &Token{
		content:  string(line[tokenizer.currentPos-2 : tokenizer.currentPos-1]),
		line:     tokenizer.currentLine,
		startPos: tokenizer.currentPos - 2,
		endPos:   tokenizer.currentPos - 1,
		tp:       CharacterTP,
	}, nil
}

func (tokenizer *Tokenizer) tokenDivide(line []byte) (*Token, error) {
	token := &Token{
		content:  "/",
		line:     tokenizer.currentLine,
		tp:       DivideTP,
		startPos: tokenizer.currentPos,
		endPos:   tokenizer.currentPos + 1,
	}
	tokenizer.currentPos++
	return token, nil
}

func (tokenizer *Tokenizer) tokenSingleLineComment(line []byte) (*Token, error) {
	token := &Token{
		content:  string(line[tokenizer.currentPos:]),
		line:     tokenizer.currentLine,
		tp:       SingleLineCommentTP,
		startPos: tokenizer.currentPos,
		endPos:   tokenizer.currentPos + len(line),
	}
	tokenizer.currentPos += len(line)
	return token, nil
}

func (tokenizer *Tokenizer) tokenMultipleLineOpenComment(line []byte) (*Token, error) {
	token := &Token{
		content:  string(line[tokenizer.currentPos : tokenizer.currentPos+2]),
		line:     tokenizer.currentLine,
		tp:       MultipleLineOpenCommentTP,
		startPos: tokenizer.currentPos,
		endPos:   tokenizer.currentPos + 2,
	}
	tokenizer.currentPos += 2
	return token, nil
}

func (tokenizer *Tokenizer) tokenString(line []byte) (*Token, error) {
	// Looking forward through line to find a closing quote.
	startPos := tokenizer.currentPos
	tokenizer.currentPos++
	foundClosingQuote := false
	for tokenizer.currentPos < len(line) {
		if line[tokenizer.currentPos] == '"' {
			tokenizer.currentPos++
			foundClosingQuote = true
			break
		}
		tokenizer.currentPos++
	}
	// If cannot find an closing quote, then string format is not correct.
	if !foundClosingQuote {
		return nil, tokenizer.makeError(string(line), tokenizer.currentLine, "incorrect string format")
	}
	token := &Token{
		content:  string(line[startPos+1 : tokenizer.currentPos-1]),
		line:     tokenizer.currentLine,
		startPos: startPos + 1,
		endPos:   tokenizer.currentPos - 1,
		tp:       StringTP,
	}
	return token, nil
}

func (tokenizer *Tokenizer) tokenNumber(line []byte) (*Token, error) {
	// Look forward to find a continuous number
	startPos := tokenizer.currentPos
	tokenizer.currentPos++
	for tokenizer.currentPos < len(line) {
		if util.IsNumber(line[tokenizer.currentPos]) {
			tokenizer.currentPos++
			continue
		}
		break
	}
	// It's better to check whether the current is a whiteSpace, \n, //, /*.
	// But we don't check here. Because the parser, or the syntax of the language
	// can check this.
	token := &Token{
		content:  string(line[startPos:tokenizer.currentPos]),
		line:     tokenizer.currentLine,
		tp:       IntegerTP,
		startPos: startPos,
		endPos:   tokenizer.currentPos,
	}
	return token, nil
}

func (tokenizer *Tokenizer) toKeywordOrIdentifier(line []byte) (*Token, error) {
	// Look forward to find a continuous characters.
	startPos := tokenizer.currentPos
	for tokenizer.currentPos < len(line) {
		if util.IsLetterOrUnderscoreOrNumber(line[tokenizer.currentPos]) {
			tokenizer.currentPos++
			continue
		}
		break
	}
	tokenBytes := line[startPos:tokenizer.currentPos]
	token, isKeyWord := tokenizer.tryTransformToKeyWord(tokenBytes)
	if isKeyWord {
		token.startPos, token.endPos = startPos, tokenizer.currentPos
		return token, nil
	}
	token, err := tokenizer.tryTransformToIdentifier(tokenBytes)
	if err != nil {
		return nil, err
	}
	token.startPos, token.endPos = startPos, tokenizer.currentPos
	return token, nil
}

func (tokenizer *Tokenizer) tryTransformToKeyWord(tokenBytes []byte) (*Token, bool) {
	keyWordTP, isKeyWord := keyWordTokenTPMap[string(tokenBytes)]
	if !isKeyWord {
		return nil, false
	}
	return &Token{
		content: string(tokenBytes),
		line:    tokenizer.currentLine,
		tp:      keyWordTP,
	}, true
}

func (tokenizer *Tokenizer) tryTransformToIdentifier(tokenBytes []byte) (*Token, error) {
	if util.IsNumber(tokenBytes[0]) {
		return nil, tokenizer.makeError(string(tokenBytes), tokenizer.currentLine, "incorrect identifier format")
	}
	return &Token{
		content: string(tokenBytes),
		line:    tokenizer.currentLine,
		tp:      IdentifierTP,
	}, nil
}

func (tokenizer *Tokenizer) makeError(near string, line int, msg string) error {
	return errors.New(fmt.Sprintf("Tokenizer: tokenizer error near %s at line %d, msg: %s", near, line, msg))
}

// Tokenizer accepts a source `rd` and tokenizer it's content according to hack language rules.
// This method is the main method of this tokenizer.
func (tokenizer *Tokenizer) Tokenize(rd io.Reader) (tokens []*Token, err error) {
	bfReader := bufio.NewReader(rd)
	tokenizer.currentLine = 0
	for {
		tokenizer.currentLine++
		tokenizer.currentPos = 0
		line, err := bfReader.ReadBytes('\n')
		if err != nil && err != io.EOF {
			return nil, err
		}
		for tokenizer.currentPos < len(line) {
			err, match := tokenizer.parseLine(line)
			if err != nil {
				return nil, err
			}
			line, err = tokenizer.lookForwardForMatchingMultipleLineComment(bfReader, line, !match)
			if err != nil {
				return nil, err
			}
		}
		if err == io.EOF {
			return tokenizer.tokens, nil
		}
	}
}

func (tokenizer *Tokenizer) parseLine(line []byte) (error, bool) {
	for {
		token, err := tokenizer.getNextToken(line)
		if err != nil {
			return err, true
		}
		if token == nil {
			return nil, true
		}
		switch token.tp {
		case MultipleLineOpenCommentTP:
			match := tokenizer.lookForwardForMatchingMultipleLineCommentAtCurrentLine(line)
			if !match {
				return nil, false
			}
			continue
		case SingleLineCommentTP:
			return nil, true
		default:
			tokenizer.tokens = append(tokenizer.tokens, token)
		}
		if err != nil {
			return err, true
		}
	}
}

func (tokenizer *Tokenizer) lookForwardForMatchingMultipleLineComment(bfReader *bufio.Reader, line []byte, needFindMatch bool) ([]byte, error) {
	if !needFindMatch {
		return line, nil
	}
	var err error
	startLine := tokenizer.currentLine
	startContent := string(line)
	for {
		match := tokenizer.lookForwardForMatchingMultipleLineCommentAtCurrentLine(line)
		if match {
			return line, nil
		}
		// If cannot find the matching closing multiple line comment at current line.
		// We skip to next line to find.
		line, err = bfReader.ReadBytes('\n')
		if err != nil && err != io.EOF {
			return nil, err
		}
		if err == io.EOF || len(line) == 0 {
			return nil, tokenizer.makeError(startContent, startLine, "incorrect comment format.")
		}
		tokenizer.currentLine++
		tokenizer.currentPos = 0
	}
}

func (tokenizer *Tokenizer) lookForwardForMatchingMultipleLineCommentAtCurrentLine(line []byte) bool {
	for tokenizer.currentPos < len(line) {
		// If it's /*
		if tokenizer.currentPos < len(line)-1 && line[tokenizer.currentPos] == '/' &&
			line[tokenizer.currentPos+1] == '*' {
			tokenizer.currentPos += 2
			continue
		}
		// If it's */
		if tokenizer.currentPos < len(line)-1 && line[tokenizer.currentPos] == '*' &&
			line[tokenizer.currentPos+1] == '/' {
			tokenizer.currentPos += 2
			return true
		}
		// Otherwise we look forward.
		tokenizer.currentPos++
	}
	return false
}

func (tokenizer *Tokenizer) Reset() {
	tokenizer.currentPos, tokenizer.currentLine, tokenizer.currentFile = 0, 0, ""
	tokenizer.tokens = nil
}

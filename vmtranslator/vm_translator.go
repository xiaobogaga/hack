package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// A simple vm translator to transform vm language to hack assembler code.

// There are four kinds of vm commands, they are:
// * Arithmetic commands
// * Memory access commands
// * Program flow commands
// * Function calling commands

// Arithmetic commands: add, sub, neq, eq, gt, lt, and, or, not.
// Memory Access commands: push segment index, pop segment index, where segment can be
// argument, local, static, constant, this, that, pointer, temp.
// Program flow commands are: label label, if-goto label_name, goto label
// Function calling commands: function f n, call f m, return.

// For each commands, the task of VMTranslator is to transform them to the
// corresponding hack assembler code.

// We need to know how to map those vm commands to assembler code. For arithmetic commands, they pop operands
// from the stack, and play the operation on them. So what is the stack structure and how to get an element from
// them.

// The first task of VMTranslator is to parse the files to tokenize all words. All possible syntax are:
// Memory access commands: push|pop segments integer where segments could be: [argument, local, static, constant, this, that, pointer, temp].
// Arithmetic commands: add, sub, neq, eq, gt, lt, and, or, not
// Program flow commands: label label_name, if-goto label_name, goto label_name. where label_name is a-zA-Z._:
// Function calling commands: function func_name integer, call func_name integer, return.

type KeyWordTP int

const (
	PushKeyWordTP KeyWordTP = iota
	PopKeyWordTP
	ArgumentKeyWordTP
	LocalKeyWordTP
	StaticKeyWordTP
	ConstantKeyWordTP
	ThisKeyWordTP
	ThatKeyWordTP
	PointerKeyWordTP
	TempKeyWordTP
	AddKeyWordTP
	SubKeyWordTP
	NegKeyWordTP
	EqKeyWordTP
	GtKeyWordTP
	LtKeyWordTP
	AndKeyWordTP
	OrKeyWordTP
	NotKeyWordTP
	LabelKeyWordTP
	IfGotoKeyWordTP
	GotoKeyWordTP
	FunctionKeyWordTP
	CallKeyWordTP
	ReturnKeyWordTP
	CommentKeyWordTP
)

var keyWordsMap = map[string]KeyWordTP{
	"PUSH":     PushKeyWordTP,
	"POP":      PopKeyWordTP,
	"ARGUMENT": ArgumentKeyWordTP,
	"LOCAL":    LocalKeyWordTP,
	"STATIC":   StaticKeyWordTP,
	"CONSTANT": ConstantKeyWordTP,
	"THIS":     ThisKeyWordTP,
	"THAT":     ThatKeyWordTP,
	"POINTER":  PointerKeyWordTP,
	"TEMP":     TempKeyWordTP,
	"ADD":      AddKeyWordTP,
	"SUB":      SubKeyWordTP,
	"NEG":      NegKeyWordTP,
	"EQ":       EqKeyWordTP,
	"GT":       GtKeyWordTP,
	"LT":       LtKeyWordTP,
	"AND":      AndKeyWordTP,
	"OR":       OrKeyWordTP,
	"NOT":      NotKeyWordTP,
	"LABEL":    LabelKeyWordTP,
	"IF-GOTO":  IfGotoKeyWordTP,
	"GOTO":     GotoKeyWordTP,
	"FUNCTION": FunctionKeyWordTP,
	"CALL":     CallKeyWordTP,
	"RETURN":   ReturnKeyWordTP,
	"//":       CommentKeyWordTP,
}

type VMTranslator struct {
	fileName        string
	lineCounter     int
	output          bytes.Buffer
	labelNameID     int
	funcCallID      int
	currentFunction string
}

func NewVMTranslator() *VMTranslator {
	return &VMTranslator{output: bytes.Buffer{}}
}

func (translator *VMTranslator) translateProgram(path string, writeInitializeCode bool) error {
	if writeInitializeCode {
		translator.writeInitializeCode()
	}
	files, err := ioutil.ReadDir(path)
	if err != nil {
		return err
	}
	for _, f := range files {
		// Ignore sub path
		if f.IsDir() {
			continue
		}
		fName := f.Name()
		// Ignore not vm file
		if len(fName) < 4 || (len(fName) >= 4 && fName[len(fName)-3:] != ".vm") {
			continue
		}
		err := translator.translateFile(path, f.Name())
		if err != nil {
			return err
		}
	}
	return nil
}

func (translator *VMTranslator) translateFile(path, filename string) error {
	translator.fileName = filename
	translator.lineCounter = 0
	rd, err := os.Open(path + "/" + filename)
	if err != nil {
		return err
	}
	return translator.Parse(rd)
}

func (translator *VMTranslator) Parse(rd io.Reader) error {
	reader := bufio.NewReader(rd)
	for {
		line, err := reader.ReadBytes('\n')
		if err != nil && err != io.EOF {
			return err
		}
		if err == io.EOF {
			break
		}
		err = translator.parseLine(line)
		if err != nil {
			return err
		}
		translator.lineCounter++
	}
	return nil
}

// writeInitializeCode to bootstrap program. which are:
// SP=256
// call Main.main 0
// ($InfiniteLoop)
// @$InfiniteLoop
// 0;JMP
func (translator *VMTranslator) writeInitializeCode() {
	translator.output.WriteString(`
		// For write initialize code.
		@256
		D=A
		@SP
		M=D
		`)
	translator.parseCall([]byte("Sys.init 0"))
	translator.output.WriteString(`
		($InfiniteLoop)
		@$InfiniteLoop
		0;JMP
		`)
}

// getNextToken tries to fetch the next token, and return it if it has, otherwise return an empty string, and .
func (translator *VMTranslator) getNextToken(line []byte) (string, []byte) {
	line = bytes.TrimSpace(line)
	for i := 0; i < len(line); i++ {
		c := line[i]
		if c == ' ' || c == '	' || c == '\n' || c == '\r' || c == '\f' || c == '\v' {
			return string(line[:i]), line[i:]
		}
	}
	return string(line), nil
}

func (translator *VMTranslator) parseLine(line []byte) (err error) {
	token, line := translator.getNextToken(line)
	if len(token) == 0 {
		return nil
	}
	keyWordTP, exist := keyWordsMap[strings.ToUpper(token)]
	if !exist {
		return translator.makeError(token)
	}
	switch keyWordTP {
	case CommentKeyWordTP:
		translator.parseComment()
	case PushKeyWordTP:
		err = translator.parsePush(line)
	case PopKeyWordTP:
		err = translator.parsePop(line)
	case AddKeyWordTP:
		err = translator.parseAdd(line)
	case SubKeyWordTP:
		err = translator.parseSub(line)
	case NegKeyWordTP:
		err = translator.parseNeg(line)
	case EqKeyWordTP:
		err = translator.parseEq(line)
	case GtKeyWordTP:
		err = translator.parseGt(line)
	case LtKeyWordTP:
		err = translator.parseLt(line)
	case AndKeyWordTP:
		err = translator.parseAnd(line)
	case OrKeyWordTP:
		err = translator.parseOr(line)
	case NotKeyWordTP:
		err = translator.parseNot(line)
	case LabelKeyWordTP:
		err = translator.parseLabel(line)
	case IfGotoKeyWordTP:
		err = translator.parseIfGoto(line)
	case GotoKeyWordTP:
		err = translator.parseGoto(line)
	case FunctionKeyWordTP:
		err = translator.parseFunction(line)
	case CallKeyWordTP:
		err = translator.parseCall(line)
	case ReturnKeyWordTP:
		err = translator.parseReturn(line)
	default:
		err = translator.makeError(token)
	}
	return err
}

func (translator *VMTranslator) parseComment() {
	return
}

func (translator *VMTranslator) parsePush(line []byte) (err error) {
	token, line := translator.getNextToken(line)
	if len(token) == 0 {
		return translator.makeError(token)
	}
	keyWordTP, exist := keyWordsMap[strings.ToUpper(token)]
	if !exist {
		return translator.makeError(token)
	}
	switch keyWordTP {
	case ArgumentKeyWordTP:
		line, err = translator.parseArgument(PushKeyWordTP, line)
	case LocalKeyWordTP:
		line, err = translator.parseLocal(PushKeyWordTP, line)
	case StaticKeyWordTP:
		line, err = translator.parseStatic(PushKeyWordTP, line)
	case ConstantKeyWordTP:
		line, err = translator.parseConstant(PushKeyWordTP, line)
	case ThisKeyWordTP:
		line, err = translator.parseThis(PushKeyWordTP, line)
	case ThatKeyWordTP:
		line, err = translator.parseThat(PushKeyWordTP, line)
	case PointerKeyWordTP:
		line, err = translator.parsePointer(PushKeyWordTP, line)
	case TempKeyWordTP:
		line, err = translator.parseTemp(PushKeyWordTP, line)
	default:
		err = translator.makeError(token)
	}
	if err != nil {
		return err
	}
	return translator.parseRemainContent(line)
}

func (translator *VMTranslator) parsePop(line []byte) (err error) {
	token, line := translator.getNextToken(line)
	if len(token) == 0 {
		return translator.makeError(token)
	}
	keyWordTP, exist := keyWordsMap[strings.ToUpper(token)]
	if !exist {
		return translator.makeError(token)
	}
	switch keyWordTP {
	case ArgumentKeyWordTP:
		line, err = translator.parseArgument(PopKeyWordTP, line)
	case LocalKeyWordTP:
		line, err = translator.parseLocal(PopKeyWordTP, line)
	case StaticKeyWordTP:
		line, err = translator.parseStatic(PopKeyWordTP, line)
	case ConstantKeyWordTP:
		line, err = translator.parseConstant(PopKeyWordTP, line)
	case ThisKeyWordTP:
		line, err = translator.parseThis(PopKeyWordTP, line)
	case ThatKeyWordTP:
		line, err = translator.parseThat(PopKeyWordTP, line)
	case PointerKeyWordTP:
		line, err = translator.parsePointer(PopKeyWordTP, line)
	case TempKeyWordTP:
		line, err = translator.parseTemp(PopKeyWordTP, line)
	default:
		err = translator.makeError(token)
	}
	if err != nil {
		return err
	}
	return translator.parseRemainContent(line)
}

func (translator *VMTranslator) getIntegerValue(line []byte) (int, []byte, error) {
	token, line := translator.getNextToken(line)
	if len(token) == 0 {
		return -1, nil, translator.makeError(token)
	}
	ret, err := strconv.Atoi(token)
	if err != nil {
		return -1, nil, translator.makeError(token)
	}
	return ret, line, nil
}

// parseArgument is used to parse PUSH|POP ARGUMENT index commands. In hack standard, ARG symbol in
// hack assembler code is used to refer the Argument segment location of current function.
// Lets see some examples. PUSH ARGUMENT 1, will get the value of *(*ARG+1) and push this value to current
// stack. POP ARGUMENT 1 will pop current value from stack and store this value in (*arg+1).
// we can transform this procedure to a serial hack assembler commands:
// // For Push Argument $value
// @value
// D=A
// @ARG
// A=M+D
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP ARGUMENT $value
// @value
// D=A
// @ARG
// D=M+D
// @$segment_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$segment_index_loc
// A=M
// M=D // store top most element to segment[index]
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseArgument(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push Argument %d
			@%d
			D=A
			@ARG
			A=M+D
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP ARGUMENT %d
			@%d
			D=A
			@ARG
			D=M+D
			@$segment_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$segment_index_loc
			A=M
			M=D // store top most element to segment[index]
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Similar to `parseArgument` and for the transformed assembler code, just need to replace `ARG` to LCL
// // For Push Local $value
// @value
// D=A
// @LCL
// A=M+D
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP Local $value
// @value
// D=A
// @LCL
// D=M+D
// @$segment_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$segment_index_loc
// A=M
// M=D // store top most element to segment[index]
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseLocal(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push Local %d
			@%d
			D=A
			@LCL
			A=M+D
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP Local %d
			@%d
			D=A
			@LCL
			D=M+D
			@$segment_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$segment_index_loc
			A=M
			M=D // store top most element to segment[index]
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Similar to `parseArgument` and for the transformed assembler code, just need to replace `ARG` to THIS
// // For Push THIS $value
// @value
// D=A
// @THIS
// A=M+D
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP THIS $value
// @value
// D=A
// @THIS
// D=M+D
// @$segment_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$segment_index_loc
// A=M
// M=D // store top most element to segment[index]
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseThis(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push THIS %d
			@%d
			D=A
			@THIS
			A=M+D
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP THIS %d
			@%d
			D=A
			@THIS
			D=M+D
			@$segment_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$segment_index_loc
			A=M
			M=D // store top most element to segment[index]
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Similar to `parseArgument` and for the transformed assembler code, just need to replace `ARG` to THAT
// // For Push THAT $value
// @value
// D=A
// @THAT
// A=M+D
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP THAT $value
// @value
// D=A
// @THAT
// D=M+D
// @$segment_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$segment_index_loc
// A=M
// M=D // store top most element to segment[index]
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseThat(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push THAT %d
			@%d
			D=A
			@THAT
			A=M+D
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP THAT %d
			@%d
			D=A
			@THAT
			D=M+D
			@$segment_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$segment_index_loc
			A=M
			M=D // store top most element to segment[index]
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Static variables can be transformed to vm variables. For example. PUSH STATIC index can be transformed to:
// // For Push static $index
// @$file.index
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For Pop static $index
// @SP
// A=M-1
// D=M // top most element.
// @$file.index
// M=D
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseStatic(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push static %d
			@$%s.%d
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, translator.fileName, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For Pop static %d
			@SP
			A=M-1
			D=M // top most element.
			@$%s.%d
			M=D
			@SP
			M=M-1 //SP=SP-1
			`, value, translator.fileName, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Access pointer i can be seen as access 3 + i.
// // For Push Pointer $value
// @value
// D=A
// @3
// A=D+A
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP Pointer $value
// @value
// D=A
// @3
// D=D+A
// @$segment_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$segment_index_loc
// A=M
// M=D // store top most element to pointers
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parsePointer(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push Pointer %d
			@%d
			D=A
			@3
			A=D+A
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP Pointer %d
			@%d
			D=A
			@3
			D=D+A
			@$segment_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$segment_index_loc
			A=M
			M=D // store top most element to pointers
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Access temp i can be seen as access 5 + i.
// // For Push Temp $value
// @value
// D=A
// @5
// A=D+A
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For POP Temp $value
// @value
// D=A
// @5
// D=D+A
// @$temp_index_loc
// M=D
// @SP
// A=M-1
// D=M // top most element.
// @$temp_index_loc
// A=M
// M=D // store top most element to temp
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseTemp(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push Temp %d
			@%d
			D=A
			@5
			A=D+A
			D=M
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For POP Temp %d
			@%d
			D=A
			@5
			D=D+A
			@$temp_index_loc
			M=D
			@SP
			A=M-1
			D=M // top most element.
			@$temp_index_loc
			A=M
			M=D // store top most element to temp
			@SP
			M=M-1 //SP=SP-1
			`, value, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Constant are a constant. For example: PUSH CONSTANT 10 means put constant 10 to stack.
// And note: POP CONSTANT 10, will pop an element from stack and because constant segment are virtual
// it does nothing for constant segment.
// // For Push constant $value
// @value
// D=A
// @SP
// A=M
// M=D
// @SP
// M=M+1 // SP=SP+1
// // For Pop constant $value
// @SP
// M=M-1 //SP=SP-1
func (translator *VMTranslator) parseConstant(opTP KeyWordTP, line []byte) ([]byte, error) {
	value, line, err := translator.getIntegerValue(line)
	if err != nil {
		return nil, err
	}
	switch opTP {
	case PushKeyWordTP:
		out := fmt.Sprintf(`
			// For Push constant %d
			@%d
			D=A
			@SP
			A=M
			M=D
			@SP
			M=M+1 // SP=SP+1
			`, value, value)
		translator.output.WriteString(out)
	case PopKeyWordTP:
		out := fmt.Sprintf(`
			// For Pop constant %d
			@SP
			M=M-1 //SP=SP-1
			`, value)
		translator.output.WriteString(out)
	default:
		return nil, translator.makeError(strconv.FormatInt(int64(value), 10))
	}
	return line, nil
}

// Add or other arithmetic commands are used to calculate the result of topmost elements on stack, they pop
// such elements first, calculate the result, then push the result to stack. They share many common aspects,
// except the unary and binary properties. Take add as an example, Add can be transformed to such assembler
// commands:
// // For Add
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=D+A   // D=D+A
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseAdd(line []byte) error {
	translator.output.WriteString(`
		// For Add
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=D+A   // D=D+A
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Sub
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=A-D   // D=A-D
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseSub(line []byte) error {
	translator.output.WriteString(`
		// For Sub
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=A-D   // D=A-D
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Neg
// @SP
// A=M-1
// D=M     // D = topmost.
// D=-D    // D=-D
// @SP
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseNeg(line []byte) error {
	translator.output.WriteString(`
		// For Neg
		@SP
		A=M-1
		D=M     // D = topmost.
		D=-D    // D=-D
		@SP
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Eq
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=D-A   // D=D-A
// @$set_d_true_unique_label
// D;JEQ
// D=0
// @$jump_to_set_sp_unique_label
// 0;JMP
// ($set_d_true_unique_label)
// D=-1
// ($jump_to_set_sp_unique_label)
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseEq(line []byte) error {
	translator.output.WriteString(fmt.Sprintf(`
		// For Eq
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=D-A   // D=D-A
		@$set_d_true_%d
		D;JEQ
		D=0
		@$jump_to_set_sp_%d
		0;JMP
		($set_d_true_%d)
		D=-1
		($jump_to_set_sp_%d)
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`, translator.labelNameID, translator.labelNameID, translator.labelNameID, translator.labelNameID))
	translator.labelNameID++
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Gt
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=A-D   // D=A-D
// @$set_d_true_unique_label
// D;JGT
// D=0
// @$jump_to_set_sp_unique_label
// 0;JMP
// ($set_d_true_unique_label)
// D=-1
// ($jump_to_set_sp_unique_label)
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseGt(line []byte) error {
	translator.output.WriteString(fmt.Sprintf(`
		// For Gt
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=A-D   // D=A-D
		@$set_d_true_%d
		D;JGT
		D=0
		@$jump_to_set_sp_%d
		0;JMP
		($set_d_true_%d)
		D=-1
		($jump_to_set_sp_%d)
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`, translator.labelNameID, translator.labelNameID, translator.labelNameID, translator.labelNameID))
	translator.labelNameID++
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Lt
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=A-D   // D=A-D
// @$set_d_true_unique_label
// D;JLT
// D=0
// @$jump_to_set_sp_unique_label
// 0;JMP
// ($set_d_true_unique_label)
// D=-1
// ($jump_to_set_sp_unique_label)
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseLt(line []byte) error {
	translator.output.WriteString(fmt.Sprintf(`
		// For Lt
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=A-D   // D=A-D
		@$set_d_true_%d
		D;JLT
		D=0
		@$jump_to_set_sp_%d
		0;JMP
		($set_d_true_%d)
		D=-1
		($jump_to_set_sp_%d)
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`, translator.labelNameID, translator.labelNameID, translator.labelNameID, translator.labelNameID))
	translator.labelNameID++
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For And
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=D&A   // D=D&A
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseAnd(line []byte) error {
	translator.output.WriteString(`
		// For And
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=D&A   // D=D&A
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Or
// @SP
// A=M-1
// D=M     // D = topmost.
// A=A-1
// A=M     // A = second topmost
// D=D|A   // D=D|A
// @SP
// M=M-1   // SP=SP-1
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseOr(line []byte) error {
	translator.output.WriteString(`
		// For Or
		@SP
		A=M-1
		D=M     // D = topmost.
		A=A-1
		A=M     // A = second topmost
		D=D|A   // D=D|A
		@SP
		M=M-1   // SP=SP-1
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

// The transformed command:
// // For Not
// @SP
// A=M-1
// D=!M     // D = topmost.
// @SP
// A=M-1
// M=D     // push the result to the stack.
func (translator *VMTranslator) parseNot(line []byte) error {
	translator.output.WriteString(`
		// For Not
		@SP
		A=M-1
		D=!M     // D = topmost.
		@SP
		A=M-1
		M=D     // push the result to the stack.
		`)
	return translator.parseRemainContent(line)
}

var labelFormat = regexp.MustCompile("^[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:][0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.$:]*")

func (translator *VMTranslator) parseLabelName(line []byte) ([]byte, string, error) {
	token, line := translator.getNextToken(line)
	if len(token) == 0 {
		return nil, "", translator.makeError(token)
	}
	if !labelFormat.Match([]byte(token)) {
		return nil, "", translator.makeError(token)
	}
	return line, token, nil
}

// Label $labelName can transform to a assembler code:
// // For Label $labelName
// ($funcName_label)
func (translator *VMTranslator) parseLabel(line []byte) error {
	line, label, err := translator.parseLabelName(line)
	if err != nil {
		return err
	}
	translator.output.WriteString(fmt.Sprintf(`
		// For Label %s
		(%s_%s)
		`, label, translator.currentFunction, label))
	return translator.parseRemainContent(line)
}

// If-Goto $labelName command can be transformed to such assembler code:
// // For IF-GOTO $labelName
// @SP
// A=M-1
// D=M  // D is topmost
// @SP
// M=M-1 // SP=SP-1
// @funcName_label
// D;JNE
func (translator *VMTranslator) parseIfGoto(line []byte) error {
	line, label, err := translator.parseLabelName(line)
	if err != nil {
		return err
	}
	translator.output.WriteString(fmt.Sprintf(`
		// For IF-GOTO %s
		@SP
		A=M-1
		D=M  // D is topmost
		@SP
		M=M-1 // SP=SP-1
		@%s_%s
		D;JNE
		`, label, translator.currentFunction, label))
	return translator.parseRemainContent(line)
}

// Goto $labelName command can be transformed to such assembler code:
// // For Goto $labelName
// @funcName_label
// 0;JMP
func (translator *VMTranslator) parseGoto(line []byte) error {
	line, label, err := translator.parseLabelName(line)
	if err != nil {
		return err
	}
	translator.output.WriteString(fmt.Sprintf(`
		// For Goto %s
		@%s_%s
		0;JMP
		`, label, translator.currentFunction, label))
	return translator.parseRemainContent(line)
}

// function f k, declare a function f which has k local variables.
// assembler code:
// // For function f k
// (funcName)
// @k
// D=A
// @$func_local_var_nums
// M=D
// ($funcName_local_var_loop) // push 0 k times.
// // push 0
// @$func_local_var_nums
// D=M
// @$funcName_local_var_loop_exit // exit if k == 0
// D;JEQ
// // otherwise we push 0 and k = k - 1
// @SP
// A=M
// M=0
// @SP
// M=M+1 // inc SP
// @$func_local_var_nums
// M=M-1 // k=k-1
// @$funcName_local_var_loop
// 0;JMP
// ($funcName_local_var_loop_exit)
//
func (translator *VMTranslator) parseFunction(line []byte) error {
	line, funcName, err := translator.parseLabelName(line)
	if err != nil {
		return err
	}
	localVariableSizeK, line, err := translator.getIntegerValue(line)
	if err != nil {
		return err
	}
	translator.output.WriteString(fmt.Sprintf(`
		// For function %s %d
		(%s)
		@%d
		D=A
		@$func_local_var_nums
		M=D
		($%s_local_var_loop) // push 0 k times.
		// push 0
		@$func_local_var_nums
		D=M
		@$%s_local_var_loop_exit // exit if k == 0
		D;JEQ
		// otherwise we push 0 and k = k - 1
		@SP
		A=M
		M=0
		@SP
		M=M+1 // inc SP
		@$func_local_var_nums
		M=M-1 // k=k-1
		@$%s_local_var_loop
		0;JMP
		($%s_local_var_loop_exit)
		`, funcName, localVariableSizeK, funcName, localVariableSizeK, funcName, funcName, funcName, funcName))
	// Important: we need to set function name for commands like: lt, gt.
	translator.currentFunction = funcName
	return translator.parseRemainContent(line)
}

// call f n. Calling a function f with n arguments have been pushed to stack.
// assembler codes:
// // For call f n
// // push return-address
// @$callingFunc_indicator_return_address_id_return_address // this is unique.
// D=A
// @SP
// A=M
// M=D
// @SP
// M=M+1
// // push LCL
// @LCL
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1
// // push ARG
// @ARG
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1
// // push THIS
// @THIS
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1
// // push THAT
// @THAT
// D=M
// @SP
// A=M
// M=D
// @SP
// M=M+1
// // set ARG=SP-n-5
// @n
// D=A
// @5
// D=D+A
// @SP
// D=M-D
// @ARG
// M=D
// // set LCL=SP
// @SP
// D=M
// @LCL
// M=D
// // goto f
// @funcName
// 0;JMP
// ($callingFunc_indicator_return_address_id_return_address)
func (translator *VMTranslator) parseCall(line []byte) error {
	line, funcName, err := translator.parseLabelName(line)
	if err != nil {
		return err
	}
	argumentSizeN, line, err := translator.getIntegerValue(line)
	if err != nil {
		return err
	}
	translator.output.WriteString(fmt.Sprintf(`
		// For call %s %d
		// push return-address
		@$%s_%d_return_address_id_return_address // this is unique.
		D=A
		@SP
		A=M
		M=D
		@SP
		M=M+1
		// push LCL
		@LCL
		D=M
		@SP
		A=M
		M=D
		@SP
		M=M+1
		// push ARG
		@ARG
		D=M
		@SP
		A=M
		M=D
		@SP
		M=M+1
		// push THIS
		@THIS
		D=M
		@SP
		A=M
		M=D
		@SP
		M=M+1
		// push THAT
		@THAT
		D=M
		@SP
		A=M
		M=D
		@SP
		M=M+1
		// set ARG=SP-n-5
		@%d
		D=A
		@5
		D=D+A
		@SP
		D=M-D
		@ARG
		M=D
		// set LCL=SP
		@SP
		D=M
		@LCL
		M=D
		// goto f
		@%s
		0;JMP
		($%s_%d_return_address_id_return_address)
		`, funcName, argumentSizeN, funcName, translator.funcCallID, argumentSizeN, funcName, funcName,
		translator.funcCallID))
	translator.funcCallID++
	return translator.parseRemainContent(line)
}

// Return. can be transformed to such assembler codes:
// Return value have been pushed to stack, put it to caller SP
// // For Return
// @5
// D=A
// @LCL
// A=M-D
// D=M
// @$return_address
// M=D
// @SP
// A=M-1
// D=M
// @ARG
// A=M
// M=D // *ARG=pop()
// D=A
// @SP
// M=D+1 // SP=ARG+1
// // Restore previous LCL, THIS, THAT, ARG
// @LCL
// A=M-1
// D=M
// @THAT
// M=D // restore THAT
// @2
// D=A
// @LCL
// A=M-D
// D=M
// @THIS
// M=D // restore THIS
// @3
// D=A
// @LCL
// A=M-D
// D=M
// @ARG
// M=D // restore ARG
// @4
// D=A
// @LCL
// A=M-D
// D=M
// @LCL
// M=D // restore LCL
// // goto caller
// @$return_address
// A=M
// 0;JMP
func (translator *VMTranslator) parseReturn(line []byte) error {
	translator.output.WriteString(`
		// For Return
		@5
		D=A
		@LCL
		A=M-D
		D=M
		@$return_address
		M=D
		@SP
		A=M-1
		D=M
		@ARG
		A=M
		M=D // *ARG=pop()
		D=A
		@SP
		M=D+1 // SP=ARG+1
		// Restore previous LCL, THIS, THAT, ARG
		@LCL
		A=M-1
		D=M
		@THAT
		M=D // restore THAT
		@2
		D=A
		@LCL
		A=M-D
		D=M
		@THIS
		M=D // restore THIS
		@3
		D=A
		@LCL
		A=M-D
		D=M
		@ARG
		M=D // restore ARG
		@4
		D=A
		@LCL
		A=M-D
		D=M
		@LCL
		M=D // restore LCL
		// goto caller
		@$return_address
		A=M
		0;JMP
		`)
	return translator.parseRemainContent(line)
}

func (translator *VMTranslator) parseRemainContent(line []byte) (err error) {
	remain := bytes.TrimSpace(line)
	if len(remain) == 0 {
		return nil
	}
	// Ignore comment
	if len(remain) >= 2 && remain[0] == '/' && remain[1] == '/' {
		return nil
	}
	return translator.makeError(string(remain))
}

func (translator *VMTranslator) makeError(near string) error {
	return errors.New(fmt.Sprintf("SyntaxError: syntax error near %s at line %d", near, translator.lineCounter))
}

func (translator *VMTranslator) saveTo(filepath string) error {
	return ioutil.WriteFile(filepath, translator.output.Bytes(), 0666)
}

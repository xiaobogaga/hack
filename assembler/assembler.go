package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"regexp"
	"strconv"
)

// A simple assembler based on descent-parser, which can be used to parse hack assemble code and transform them into the
// hack binary code, aka the instruction supported by hack CPU.

// The most ambiguous instruction is the A instruction, A instruction is normally declared as @something, but it turns out
// it has many types:
// * @10(decimal value), put this value to the A register.
// * @label, put the instruction address of label to A register, note that the label can be used before declared.
// * @R[0-15], this is the predefined 16 registers R0-R15, each value is 0-15, and then put to A register.
// * @Variable, declare a variable by setting it's address (if not declared), and then put the data memory address of this variable to A register.

var predefinedVariables = map[string]int{
	"SP":     0,
	"LCL":    1,
	"ARG":    2,
	"THIS":   3,
	"THAT":   4,
	"R0":     0,
	"R1":     1,
	"R2":     2,
	"R3":     3,
	"R4":     4,
	"R5":     5,
	"R6":     6,
	"R7":     7,
	"R8":     8,
	"R9":     9,
	"R10":    10,
	"R11":    11,
	"R12":    12,
	"R13":    13,
	"R14":    14,
	"R15":    15,
	"SCREEN": 16384,
	"KBD":    24576,
}

var cCommandCompMap = map[string]string{
	"0":   "0101010",
	"1":   "0111111",
	"-1":  "0111010",
	"D":   "0001100",
	"A":   "0110000",
	"!D":  "0001101",
	"!A":  "0110001",
	"-D":  "0001111",
	"-A":  "0110011",
	"D+1": "0011111",
	"1+D": "0011111",
	"A+1": "0110111",
	"1+A": "0110111",
	"D-1": "0001110",
	"A-1": "0110010",
	"D+A": "0000010",
	"A+D": "0000010",
	"D-A": "0010011",
	"A-D": "0000111",
	"D&A": "0000000",
	"A&D": "0000000",
	"D|A": "0010101",
	"A|D": "0010101",
	"M":   "1110000",
	"!M":  "1110001",
	"-M":  "1110011",
	"M+1": "1110111",
	"1+M": "1110111",
	"M-1": "1110010",
	"D+M": "1000010",
	"M+D": "1000010",
	"D-M": "1010011",
	"M-D": "1000111",
	"D&M": "1000000",
	"M&D": "1000000",
	"D|M": "1010101",
	"M|D": "1010101",
}

var cCommandDestMap = map[string]string{
	"M":   "001",
	"D":   "010",
	"MD":  "011",
	"DM":  "011",
	"A":   "100",
	"AM":  "101",
	"MA":  "101",
	"AD":  "110",
	"DA":  "110",
	"AMD": "111",
	"ADM": "111",
	"DAM": "111",
	"DMA": "111",
	"MAD": "111",
	"MDA": "111",
}

var cCommandJumpMap = map[string]string{
	"JGT": "001",
	"JEQ": "010",
	"JGE": "011",
	"JLT": "100",
	"JNE": "101",
	"JLE": "110",
	"JMP": "111",
}

type Assembler struct {
	line                   int
	baseInstructionAddr    int
	baseMemoryAddr         int
	currentInstructionAddr int
	currentMemoryAddr      int
	labelLocationMap       map[string]int
	symbolLocations        []symbolLocation
	commands               []Command
	codes                  []string
}

type symbolLocation struct {
	symbol string
	line   int
}

func CreateAssembler() *Assembler {
	// Todo: we need to check whether all memory variable location is correct.
	return &Assembler{
		line:                   1,
		baseInstructionAddr:    0,
		baseMemoryAddr:         15,
		currentInstructionAddr: 0,
		currentMemoryAddr:      1,
		labelLocationMap:       map[string]int{},
	}
}

type CommandType int

const (
	ACommand_Constant CommandType = iota
	ACommand_Label
	ACommand_Variable
	CCommand
)

type Command struct {
	Tp              CommandType
	Code            string
	Line            int
	OriginalContent string
}

func (command Command) String() string {
	return fmt.Sprintf("Command: {Tp: %d, Code: %s, Line: %d, OriginalContent: %s}", command.Tp, command.Code,
		command.Line, command.OriginalContent)
}

// Parse parse the input source which is a sequence of assembler code, and transfer
// them into a sequence of binary code supported by hack computer of nand2tetries. The returned
// value is a command array where each element is a machine instruction.
func (asm *Assembler) Parse(rd io.Reader) (ret []Command, err error) {
	bfReader := bufio.NewReader(rd)
	for {
		line, err := bfReader.ReadBytes('\n')
		if err != nil && err != io.EOF {
			return nil, err
		}
		// Return if no remaining characters.
		if len(line) == 0 || err == io.EOF {
			asm.updateLabelOrVariableMap()
			return asm.commands, nil
		}
		line, hasRemainCharacter := asm.trimLine(line)
		if !hasRemainCharacter {
			asm.line++
			continue
		}
		err = asm.transformLine(line)
		if err != nil {
			return nil, err
		}
		asm.line++
	}
}

// updateLabelOrVariableMap updates those @label or @variable command. because those commands points to
// a memory address which we don't know before all labels declaration are parsed. we parse those commands at
// last.
func (asm *Assembler) updateLabelOrVariableMap() error {
	variableMemAddrMap := map[string]int{}
	for _, symbolLocation := range asm.symbolLocations {
		// Try to parse as if it's a label reference.
		symbol := symbolLocation.symbol
		line := symbolLocation.line
		labelAddr, exist := asm.labelLocationMap[symbol]
		if exist {
			asm.commands[line].Tp = ACommand_Label
			asm.commands[line].Code = asm.formatCode(labelAddr)
			continue
		}
		// Should be a variable declare command
		variableMemAddr, exist := variableMemAddrMap[symbol]
		if !exist {
			asm.commands[line].Tp = ACommand_Variable
			asm.commands[line].Code = asm.formatCode(asm.currentMemoryAddr + asm.baseMemoryAddr)
			variableMemAddrMap[symbol] = asm.currentMemoryAddr + asm.baseMemoryAddr
			asm.currentMemoryAddr++
		} else {
			asm.commands[line].Tp = ACommand_Variable
			asm.commands[line].Code = asm.formatCode(variableMemAddr)
		}
	}
	return nil
}

// trimLine will remove space from line, also remove comments if it has, then return whether those line has other characters after trimed.
func (asm *Assembler) trimLine(line []byte) ([]byte, bool) {
	line = bytes.TrimSpace(line)
	index := bytes.Index(line, []byte("//"))
	if index != -1 {
		line = line[:index]
		line = bytes.TrimSpace(line)
	}
	if len(line) == 0 {
		return nil, false
	}
	return line, true
}

func (asm *Assembler) transformLine(line []byte) error {
	switch line[0] {
	case '@':
		return asm.transformAOrVariableCommand(line)
	case '(':
		return asm.transformLabelCommand(line)
	case '/':
		return asm.transformComment(line)
	default:
		return asm.transformCCommand(line)
	}
}

var variableOrLabelFormat = regexp.MustCompile("^[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.$:][0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.$:]*")

// transformAOrVariableCommand after we recognize the current command is an A command or Variable command.
// A instruction has many types:
// * @10(decimal value), put this value to the A register.
// * @label, put the instruction address of label to A register, note that the label can be used before declared.
// * @R[0-15], this is the predefined 16 registers R0-R15, each value is 0-15, and then put to A register.
// * @Variable, declare a variable by setting it's address (if not declared), and then put the data memory address of
//   this variable to A register.
func (asm *Assembler) transformAOrVariableCommand(line []byte) error {
	// Try to parse a decimal value.
	originalContent := line
	line = line[1:]
	if line[0] >= '0' && line[0] <= '9' {
		return asm.transformADecimalCommand(originalContent)
	}
	// Try to parse a predefined symbol
	addr, exist := predefinedVariables[string(line)]
	if exist {
		asm.commands = append(asm.commands, Command{
			Tp:              ACommand_Variable,
			Code:            asm.formatCode(addr),
			Line:            asm.line,
			OriginalContent: string(originalContent),
		})
		asm.currentInstructionAddr++
		return nil
	}
	if !variableOrLabelFormat.Match(line) {
		return asm.makeSyntaxErr("wrong variable or label format")
	}
	// Put it to commands as a placeholder.
	asm.commands = append(asm.commands, Command{
		Tp:              ACommand_Label,
		Code:            string(line),
		Line:            asm.line,
		OriginalContent: string(originalContent),
	})
	// Put it to variableOrLabel map. Because a variable or label maybe referenced several times. So we put it
	// into the array.
	asm.symbolLocations = append(asm.symbolLocations, symbolLocation{
		symbol: string(line),
		line:   asm.currentInstructionAddr,
	})
	asm.currentInstructionAddr++
	return nil
}

func (asm *Assembler) transformADecimalCommand(line []byte) error {
	originalContent := line
	line = line[1:]
	value, err := strconv.Atoi(string(line))
	if err != nil {
		return asm.makeSyntaxErr("wrong decimal value format")
	}
	asm.commands = append(asm.commands, Command{
		Tp:              ACommand_Constant,
		Code:            asm.formatCode(value),
		Line:            asm.line,
		OriginalContent: string(originalContent),
	})
	asm.currentInstructionAddr++
	return nil
}

// transformLabelCommand after we recognize the current command is a label command.
// A label command is like '(label)', after we parse a label command, we remember its instruction loc
// which is the current instruction line + `asm.baseInstructionAddr` by putting it to `asm.labelLocationMap`.
func (asm *Assembler) transformLabelCommand(line []byte) error {
	line = line[1:]
	loc := bytes.IndexByte(line, ')')
	if loc == -1 || !variableOrLabelFormat.Match(line[:]) {
		return asm.makeSyntaxErr("wrong label format")
	}
	// We dont allow a label contains space. for example, ( hello ) is not allowed.
	label := string(line[:loc])
	// If such label exists, return an error
	_, exist := asm.labelLocationMap[label]
	if exist {
		return asm.makeSyntaxErr("found duplicate label")
	}
	//asm.commands = append(asm.commands, Command{
	//	Tp:              Command_Label_Declare,
	//	Code:            asm.formatCode(asm.baseInstructionAddr + asm.currentInstructionAddr),
	//	Line:            asm.line,
	//	OriginalContent: string(originalContent),
	//})
	asm.labelLocationMap[label] = asm.baseInstructionAddr + asm.currentInstructionAddr
	// Note: we dont need to inc instructionAddr here.
	return nil
}

func (asm *Assembler) transformComment(line []byte) error {
	if len(line) < 2 || line[1] != '/' {
		return asm.makeSyntaxErr("comment format not correct")
	}
	return nil
}

// transformCCommand after we recognize the current command is a C command.
// A C command supports: dest;comp;jump
func (asm *Assembler) transformCCommand(line []byte) error {
	originalContent := line
	destCodeStr, line, err := asm.parseCCommandDestCode(line)
	if err != nil {
		return err
	}
	jumpCodeStr, line, err := asm.parseCCommandJumpCode(line)
	if err != nil {
		return err
	}
	compCodeStr, err := asm.parseCCommandCompCode(line)
	if err != nil {
		return err
	}
	code := "111" + compCodeStr + destCodeStr + jumpCodeStr
	asm.commands = append(asm.commands, Command{
		Tp:              CCommand,
		Code:            code,
		Line:            asm.line,
		OriginalContent: string(originalContent),
	})
	asm.currentInstructionAddr++
	return nil
}

func (asm *Assembler) parseCCommandDestCode(line []byte) (string, []byte, error) {
	dest := bytes.IndexByte(line, '=')
	destCodeStr := "000"
	exist := false
	if dest == -1 {
		return destCodeStr, line, nil
	}
	destCodeStr, exist = cCommandDestMap[string(line[0:dest])]
	if !exist {
		return "", nil, asm.makeSyntaxErr(fmt.Sprintf("wrong c command of dest code format near %s", string(line)))
	}
	return destCodeStr, line[dest+1:], nil
}

func (asm *Assembler) parseCCommandJumpCode(line []byte) (string, []byte, error) {
	jumpCodeStr := "000"
	comp := bytes.IndexByte(line, ';')
	if comp == -1 {
		return jumpCodeStr, line, nil
	}
	jumpCodeStr, exist := cCommandJumpMap[string(line[comp+1:])]
	if !exist {
		return "", nil, asm.makeSyntaxErr(fmt.Sprintf("wrong c command of jump code format near %s", string(line)))
	}
	return jumpCodeStr, line[:comp], nil
}

func (asm *Assembler) parseCCommandCompCode(line []byte) (string, error) {
	compCodeStr, exist := cCommandCompMap[string(line)]
	if !exist {
		return "", asm.makeSyntaxErr(fmt.Sprintf("wrong c command of comp code format near %s", string(line)))
	}
	return compCodeStr, nil
}

// formatCode transfers the addr to binary code format
func (asm *Assembler) formatCode(addr int) string {
	// Only 16 bytes are supported. max number: 32765
	addr1 := addr
	code := [16]byte{'0'}
	for j := 15; j >= 0; j-- {
		code[j] = (byte)(addr&1) + (byte)('0')
		addr = addr >> 1
	}
	if addr1 < 0 {
		code[0] = '1'
	}
	return string(code[:])
}

func (asm *Assembler) makeSyntaxErr(msg string) error {
	return errors.New(fmt.Sprintf("syntax err at line %d: %s", asm.line, msg))
}

func (asm *Assembler) makeSyntaxErrAtSpecificLine(line int, msg string) error {
	return errors.New(fmt.Sprintf("syntax err at line %d: %s", line, msg))
}

func (asm *Assembler) convertCommandsToString() string {
	bf := bytes.Buffer{}
	for _, command := range asm.commands {
		bf.WriteString(fmt.Sprintf("%s\n", command))
	}
	return bf.String()
}

func (asm *Assembler) printAllCommands() {
	println(asm.convertCommandsToString())
}

func (asm *Assembler) saveMachineCodeToFile(filePath string) error {
	bf := bytes.Buffer{}
	for _, command := range asm.commands {
		bf.WriteString(fmt.Sprintf("%s\n", command.Code))
	}
	return ioutil.WriteFile(filePath, []byte(bf.String()), 0666)
}

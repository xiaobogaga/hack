package main

import (
	"bytes"
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestFormatCode(t *testing.T) {
	asm := CreateAssembler()
	testData := []struct {
		addr int
		code string
	}{
		{0, "0000000000000000"},
		{1, "0000000000000001"},
		{2, "0000000000000010"},
		{-1, "1111111111111111"},
		{-2, "1111111111111110"},
	}
	for _, data := range testData {
		assert.Equal(t, data.code, asm.formatCode(data.addr))
	}
}

func TestTransformComment(t *testing.T) {
	asm := CreateAssembler()
	line := []byte("/hello")
	assert.NotNil(t, asm.transformComment(line))
	line = []byte("//hhi")
	assert.Nil(t, asm.transformComment(line))
	line = []byte("///")
	assert.Nil(t, asm.transformComment(line))
}

func TestTransformCCommand(t *testing.T) {
	asm := CreateAssembler()
	type code struct {
		assembleCode string
		binaryCode   string
	}
	dest := []code{
		{assembleCode: "", binaryCode: "000"},
		{assembleCode: "M", binaryCode: "001"},
		{assembleCode: "D", binaryCode: "010"},
		{assembleCode: "MD", binaryCode: "011"},
		{assembleCode: "A", binaryCode: "100"},
		{assembleCode: "AM", binaryCode: "101"},
		{assembleCode: "AD", binaryCode: "110"},
		{assembleCode: "AMD", binaryCode: "111"},
	}
	comp := []code{
		{assembleCode: "0", binaryCode: "0101010"},
		{assembleCode: "1", binaryCode: "0111111"},
		{assembleCode: "-1", binaryCode: "0111010"},
		{assembleCode: "D", binaryCode: "0001100"},
		{assembleCode: "A", binaryCode: "0110000"},
		{assembleCode: "!D", binaryCode: "0001101"},
		{assembleCode: "!A", binaryCode: "0110001"},
		{assembleCode: "-D", binaryCode: "0001111"},
		{assembleCode: "-A", binaryCode: "0110011"},
		{assembleCode: "D+1", binaryCode: "0011111"},
		{assembleCode: "A+1", binaryCode: "0110111"},
		{assembleCode: "D-1", binaryCode: "0001110"},
		{assembleCode: "A-1", binaryCode: "0110010"},
		{assembleCode: "D+A", binaryCode: "0000010"},
		{assembleCode: "D-A", binaryCode: "0010011"},
		{assembleCode: "A-D", binaryCode: "0000111"},
		{assembleCode: "D&A", binaryCode: "0000000"},
		{assembleCode: "D|A", binaryCode: "0010101"},

		{assembleCode: "M", binaryCode: "1110000"},
		{assembleCode: "!M", binaryCode: "1110001"},
		{assembleCode: "-M", binaryCode: "1110011"},
		{assembleCode: "M+1", binaryCode: "1110111"},
		{assembleCode: "M-1", binaryCode: "1110010"},
		{assembleCode: "D+M", binaryCode: "1000010"},
		{assembleCode: "D-M", binaryCode: "1010011"},
		{assembleCode: "M-D", binaryCode: "1000111"},
		{assembleCode: "D&M", binaryCode: "1000000"},
		{assembleCode: "D|M", binaryCode: "1010101"},
	}
	jump := []code{
		{assembleCode: "", binaryCode: "000"},
		{assembleCode: "JGT", binaryCode: "001"},
		{assembleCode: "JEQ", binaryCode: "010"},
		{assembleCode: "JGE", binaryCode: "011"},
		{assembleCode: "JLT", binaryCode: "100"},
		{assembleCode: "JNE", binaryCode: "101"},
		{assembleCode: "JLE", binaryCode: "110"},
		{assembleCode: "JMP", binaryCode: "111"},
	}
	preCode := "111"
	for _, destCode := range dest {
		temp1 := destCode.assembleCode
		if temp1 != "" {
			temp1 = temp1 + "="
		}
		for _, compCode := range comp {
			temp2 := temp1
			temp2 = temp2 + compCode.assembleCode
			for _, jumpCode := range jump {
				temp3 := temp2
				if jumpCode.assembleCode != "" {
					temp3 = temp3 + ";"
				}
				temp3 = temp3 + jumpCode.assembleCode
				err := asm.transformCCommand([]byte(temp3))
				if err != nil {
					panic(fmt.Sprintf("%s err: %v", temp3, err))
				}
				assert.Equal(t, CCommand, asm.commands[len(asm.commands)-1].Tp, temp3)
				assert.Equal(t, preCode+compCode.binaryCode+
					destCode.binaryCode+jumpCode.binaryCode,
					asm.commands[len(asm.commands)-1].Code, temp3)
			}
		}
	}
}

func TestTransformLabelCommand(t *testing.T) {
	asm := CreateAssembler()
	line := "(5shsl)"
	assert.NotNil(t, asm.transformLabelCommand([]byte(line)))
	line = "(hel4lo._)"
	err := asm.transformLabelCommand([]byte(line))
	assert.Nil(t, err)
	assert.NotNil(t, asm.transformLabelCommand([]byte(line)))
}

func TestTransformADecimalCommand(t *testing.T) {
	line := []byte("@-1")
	asm := CreateAssembler()
	assert.Nil(t, asm.transformADecimalCommand(line))
	assert.Equal(t, ACommand_Constant, asm.commands[0].Tp)
	assert.Equal(t, "1111111111111111", asm.commands[0].Code)
	line = []byte("@10")
	assert.Nil(t, asm.transformADecimalCommand(line))
	assert.Equal(t, ACommand_Constant, asm.commands[1].Tp)
	assert.Equal(t, "0000000000001010", asm.commands[1].Code)
}

func TestAssembler_IntegrationTest(t *testing.T) {
	contents := `
// set M[11] = 10 + M[11]
@10
D=A
@11
M=M+D
@2
D=A // welcome
@i
M=D
@10
D=A
@j
M=D


// Loop M[11] = M[11] - 2 until M[11] < 0
(LOOP)
@i
D=A
@11
M=M-D // hello
@11
D=M
@END
D;JLT
@LOOP
0;JMP

(END)
@END
0;JMP
`
	asm := CreateAssembler()
	rd := bytes.NewReader([]byte(contents))
	_, err := asm.Parse(rd)
	assert.Nil(t, err)
	asm.printAllCommands()
}

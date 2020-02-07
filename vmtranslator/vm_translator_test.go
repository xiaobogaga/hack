package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestVMTranslator_Push(t *testing.T) {
	lines := []string{
		"push argument 1",
		"push argument 2",
		"push local 1",
		"push local 2",
		"push static 1",
		"push static 2",
		"push constant 1",
		"push constant 2",
		"push this 1",
		"push this 2",
		"push that 1",
		"push that 2",
		"push pointer 1",
		"push pointer 2",
		"push temp 1",
		"push temp 2",
	}
	translator := NewVMTranslator()
	for _, l := range lines {
		err := translator.parseLine([]byte(l))
		assert.Nil(t, err)
	}
}

func TestVMTranslator_Pop(t *testing.T) {
	lines := []string{
		"pop argument 1",
		"pop argument 2",
		"pop local 1",
		"pop local 2",
		"pop static 1",
		"pop static 2",
		"pop constant 1",
		"pop constant 2",
		"pop this 1",
		"pop this 2",
		"pop that 1",
		"pop that 2",
		"pop pointer 1",
		"pop pointer 2",
		"pop temp 1",
		"pop temp 2",
	}
	translator := NewVMTranslator()
	for _, l := range lines {
		err := translator.parseLine([]byte(l))
		assert.Nil(t, err)
	}
}

func TestVMTranslator_Arithmetic_Commands(t *testing.T) {
	lines := []string{
		"add",
		"sub",
		"neg",
		"eq",
		"gt",
		"lt",
		"and",
		"or",
		"not",
	}
	translator := NewVMTranslator()
	for _, l := range lines {
		err := translator.parseLine([]byte(l))
		assert.Nil(t, err)
	}
}

func TestVMTranslator_FunctionCallReturn(t *testing.T) {
	lines := []string{
		"function m 10",
		"call m 10",
		"return",
	}
	translator := NewVMTranslator()
	for _, l := range lines {
		err := translator.parseLine([]byte(l))
		assert.Nil(t, err)
	}
}

func TestNewVMTranslator_Label_IfGoto_Goto(t *testing.T) {
	lines := []string{
		"label lll",
		"if-goto ffff",
		"goto ffff",
	}
	translator := NewVMTranslator()
	for _, l := range lines {
		err := translator.parseLine([]byte(l))
		assert.Nil(t, err)
	}
}

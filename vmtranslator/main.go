package main

import (
	"flag"
	"fmt"
)

// A simple program to translate hack vm codes to hack assembler.

var (
	path    = flag.String("path", ".", "the program path")
	output  = flag.String("o", "./output.asm", "the saved path")
	verbose = flag.Bool("v", false, "whether print translate result")
	// This is for chapter 7.
	writeInitializeCode = flag.Bool("wi", true, "whether write initialize code")
)

func main() {
	flag.Parse()
	translator := NewVMTranslator()
	err := translator.translateProgram(*path, *writeInitializeCode)
	if err != nil {
		fmt.Printf("[Translator]: failed to translate program: %s, err: %v\n", *path, err)
		return
	}
	if *verbose {
		fmt.Println(translator.output.String())
	}
	err = translator.saveTo(*output)
	if err != nil {
		fmt.Printf("[Translator]: failed to save to path: %s, err: %v", *output, err)
	}
}

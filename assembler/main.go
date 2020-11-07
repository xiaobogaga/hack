package main

import (
	"flag"
	"fmt"
	"os"
)

// a simple program accepts a input assemble code file supported by hack assemble language and transforms
// the content to the corresponding hack machine language.

var (
	inputPath  = flag.String("i", "./input.asm", "the input hack assemble code file path")
	outputPath = flag.String("o", "./output.hack", "the output hack binary code file path")
	verbose    = flag.Bool("v", false, "whether print all transformed binary code")
)

func main() {
	flag.Parse()
	asm := CreateAssembler()
	f, err := os.Open(*inputPath)
	if err != nil {
		panic(fmt.Sprintf("failed to open file: %s, err: %v", *inputPath, err))
	}
	_, err = asm.Parse(f)
	if err != nil {
		panic(fmt.Sprintf("failed to parse file, err: %v", err))
	}
	if *verbose {
		asm.printAllCommands()
	}
	err = asm.saveMachineCodeToFile(*outputPath)
	if err != nil {
		panic(fmt.Sprintf("failed to save to path: %s, err: %v", *outputPath, err))
	}
}

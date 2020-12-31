package main

import (
	"flag"
	"fmt"
	"nands/compiler/internal"
)

var (
	path        = flag.String("path", ".", "the path of jack files needs to be compiled")
	skipChecker = flag.Bool("skip_check", true, "whether skip check")
)

func main() {
	flag.Parse()
	err := internal.Compile(*path, *skipChecker)
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
	}
	return
}

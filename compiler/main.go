package main

import (
	"flag"
	"fmt"
	"nands/compiler/internal"
)

var (
	path = flag.String("path", ".", "the path of jack files needs to be compiled")
)

func main() {
	err := internal.Compile(*path)
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
	}
	return
}

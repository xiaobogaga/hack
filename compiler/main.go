package main

import (
	"flag"
	"fmt"
	"softwares_for_nand_to_tetries/compiler/internal"
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

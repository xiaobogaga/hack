package main

import (
	"flag"
	"fmt"
	"nands/compiler/internal"
)

var (
	path = flag.String("path", "/Users/xiaobo.zhu/go/src/nands/compiler/test/11/Seven", "the path of jack files needs to be compiled")
)

func main() {
	flag.Parse()
	err := internal.Compile(*path)
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
	}
	return
}

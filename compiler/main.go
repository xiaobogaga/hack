package main

import (
	"flag"
	"fmt"
	"nands/compiler/internal"
)

var (
	path        = flag.String("path", "/Users/xiaobo.zhu/go/src/nands/compiler/test/11/ConvertToBin", "the path of jack files needs to be compiled")
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

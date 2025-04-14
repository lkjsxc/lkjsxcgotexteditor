package main

import (
	"fmt"
	"lkjsxcgoeditor/src/editor"
	"os"
)

func main() {
	e, result := editor.NewEditor()
	if result != editor.ResultOk {
		fmt.Println("Error initializing editor")
		os.Exit(1)
	}
	if result := e.Run(); result != editor.ResultOk {
		fmt.Println("Error running editor")
		os.Exit(1)
	}
	fmt.Println("Editor closed.")
}
package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/lexer"
)

func main() {
	switch len(os.Args) {
	case 1:
		repl()
	case 2:
		err := runFile(os.Args[1])
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(64)
		}
	default:
		fmt.Fprintln(os.Stderr, "Usage: blam [/path/to/file]")
		os.Exit(65)
	}
}

func repl() {
	scnr := bufio.NewScanner(os.Stdin)

	for {
		fmt.Print(">> ")
		if !scnr.Scan() {
			fmt.Println()
			return
		}

		_ = run(scnr.Text())
	}
}

func runFile(path string) error {
	src, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	err = run(string(src))
	if err != nil {
		return err
	}

	return nil
}

func run(src string) error {
	lex := lexer.NewLexer(src)
	tokens, err := lex.ScanTokens()
	if err != nil {
		return err
	}

	for _, token := range tokens {
		fmt.Println(token)
	}

	return nil
}

package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/ast"
	"github.com/Subarctic2796/blam/compiler"
	"github.com/Subarctic2796/blam/lexer"
	"github.com/Subarctic2796/blam/parser"
	"github.com/Subarctic2796/blam/value"
	"github.com/Subarctic2796/blam/vm"
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
	lex := lexer.NewLexer("")
	parser := parser.NewParser(nil)
	globals := make([]value.Value, 0)
	globalsTable := make(map[string]int)
	vm := vm.NewVM(globals, globalsTable)

	for {
		fmt.Print(">> ")
		if !scnr.Scan() {
			fmt.Println()
			return
		}

		// tokenize input
		line := scnr.Text()
		if len(line) == 0 {
			continue
		}
		lex.Reset(line)
		tokens, err := lex.ScanTokens()
		if err != nil {
			continue
		}

		// parse input
		parser.Reset(tokens)
		stmts, err := parser.Parse()
		if err != nil {
			continue
		}

		for _, stmt := range stmts {
			fmt.Println(stmt)
		}

		// compile to bytecode
		compiler := compiler.NewCompiler(nil, ast.FN_SCRIPT, globals, globalsTable)
		fn, err := compiler.Compile(stmts)
		if err != nil {
			continue
		}

		fmt.Println(vm.GlobalsTable)
		_ = vm.Interpret(fn)
	}
}

func runFile(path string) error {
	src, err := os.ReadFile(path)
	if err != nil {
		return err
	}

	// tokinze the input
	lex := lexer.NewLexer(string(src))
	tokens, err := lex.ScanTokens()
	if err != nil {
		return err
	}

	// build the ast
	parser := parser.NewParser(tokens)
	stmts, err := parser.Parse()
	if err != nil {
		return err
	}

	for _, stmt := range stmts {
		fmt.Println(stmt)
	}

	// compile the ast to bytecode
	compiler := compiler.NewCompiler(nil, ast.FN_SCRIPT, nil, nil)
	fn, err := compiler.Compile(stmts)
	if err != nil {
		return err
	}

	fmt.Println(fn)

	return nil
}

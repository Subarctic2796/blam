package vm

import (
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/value"
)

func PrintlnFn(a any) { fmt.Printf("\033[1;33m%s\033[0m\n", a) }

func TraceExecution(vm *VM, chunk *value.Chunk, offset int) {
	fmt.Fprintf(os.Stderr, "          ")
	for i := range vm.sp {
		fmt.Fprintf(os.Stderr, "[ %s ]", vm.stack[i])
	}
	fmt.Fprintln(os.Stderr)
	value.DisassembleInst(chunk, offset)
}

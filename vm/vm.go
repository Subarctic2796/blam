package vm

import (
	"fmt"
	"os"
	"time"

	"github.com/Subarctic2796/blam/value"
)

const (
	FRAMES_MAX = 64
	STACK_MAX  = (FRAMES_MAX * 256)
)

// used to implement function calls
type callFrame struct {
	*value.ObjClos
	ip int
	bp int
}

type VM struct {
	// for function calls
	frames   [FRAMES_MAX]callFrame
	frameCnt int
	// where values are stored for usague
	stack [STACK_MAX]value.Value
	// stack pointer
	sp      int
	Globals []value.Value
	// head of linked list of upvalues used for deduplication
	// and also to make sure that values are captured correctly
	openUpvalues *value.ObjUpvalue
}

func NewVM() *VM {
	vm := VM{
		frameCnt:     0,
		sp:           0,
		Globals:      make([]value.Value, 0),
		openUpvalues: nil,
	}

	vm.DefineNative("clock", func(argc int, args ...value.Value) value.Value {
		return value.Num(float64(time.Now().UnixMilli()) / 1000.0)
	})

	return &vm
}

func (vm *VM) DefineNative(name string, fn value.NativeFn) {
	vm.Globals = append(vm.Globals, &value.ObjNativeFn{
		Name: value.String(name),
		Fn:   fn,
	})
}

package vm

import (
	"fmt"
	"os"
	"time"

	"github.com/Subarctic2796/blam/opcode"
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
	sp int
	// the globals
	Globals []value.Value
	// maps a name to the index in the Globals slice
	GlobalsTable map[string]int
	// head of linked list of upvalues used for deduplication
	// and also to make sure that values are captured correctly
	openUpvalues *value.ObjUpvalue
}

func NewVM(globals []value.Value, globalsTable map[string]int) *VM {
	vm := VM{
		frameCnt:     0,
		sp:           0,
		Globals:      globals,
		GlobalsTable: globalsTable,
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
	vm.GlobalsTable[name] = len(vm.Globals) - 1
}

func (vm *VM) InitVM() { vm.resetStack() }

func (vm *VM) resetStack() {
	vm.sp = 0
	vm.frameCnt = 0
}

func (vm *VM) runTimeErr(format string, args ...any) error {
	err := fmt.Errorf("[RUN TIME ERROR] %s", fmt.Sprintf(format, args...))

	for i := vm.frameCnt - 1; i >= 0; i-- {
		frame := &vm.frames[i]
		inst := frame.ip - 1
		ln := frame.Chunk.GetLine(inst)
		fmt.Fprintf(os.Stderr, "[line %d] in ", ln)
		if frame.Name == "" {
			fmt.Fprintln(os.Stderr, "script")
		} else {
			fmt.Fprintf(os.Stderr, "%s()\n", frame.Name)
		}
	}

	vm.resetStack()
	return err
}

func (vm *VM) isFalsey(v value.Value) bool {
	_, nok := v.(value.Null)
	b, bok := v.(value.Bool)
	return nok || (bok && !bool(b))
}

func (vm *VM) peek(dist int) value.Value { return vm.stack[vm.sp-1-dist] }

func (vm *VM) push(v value.Value) {
	vm.stack[vm.sp] = v
	vm.sp++
}

func (vm *VM) pop() value.Value {
	vm.sp--
	return vm.stack[vm.sp]
}

func (vm *VM) call(fn *value.ObjClos, argc int) error {
	if argc != fn.Arity {
		return vm.runTimeErr("Expected %d arguments but got %d", fn.Arity, argc)
	}

	if vm.frameCnt == FRAMES_MAX {
		return vm.runTimeErr("Stack overflow")
	}

	vm.frameCnt++
	bp := vm.sp - argc - 1
	vm.frames[vm.frameCnt-1] = callFrame{fn, 0, bp}
	return nil
}

func (vm *VM) closeUpvalues(last int) {
	for vm.openUpvalues != nil && vm.openUpvalues.Slot >= last {
		up := vm.openUpvalues
		up.Slot = -1
		vm.openUpvalues = up.Next
	}
}

func (vm *VM) captureUpvalue(slot int) *value.ObjUpvalue {
	var prev *value.ObjUpvalue = nil
	upvalue := vm.openUpvalues
	for upvalue != nil && upvalue.Slot > slot {
		prev = upvalue
		upvalue = upvalue.Next
	}

	if upvalue != nil && upvalue.Slot == slot {
		return upvalue
	}

	// we pass the Value by value and then take a pointer to that Value
	// as this causes the go runtime to make a copy. It essentially does what
	// 'closeUpvales' does in the c version
	createdUpval := value.NewObjUpvalue(slot, vm.stack[slot])
	createdUpval.Next = upvalue

	if prev == nil {
		vm.openUpvalues = createdUpval
	} else {
		prev.Next = createdUpval
	}

	return createdUpval
}

// func (vm *VM) TraceExecution(chunk *value.Chunk, offset int) {
// 	fmt.Fprintf(os.Stderr, "          ")
// 	for i := range vm.sp {
// 		fmt.Fprintf(os.Stderr, "[ %s ]", vm.stack[i])
// 	}
// 	fmt.Fprintln(os.Stderr)
// 	value.DisassembleInst(chunk, offset)
// }

func (vm *VM) Interpret(fn *value.ObjFn) error {
	clos := value.NewObjClos(fn)
	vm.push(clos)
	_ = vm.call(clos, 0)
	return vm.Run()
}

func (vm *VM) Run() error {
	frame := &vm.frames[vm.frameCnt-1]

	readByte := func() byte {
		frame.ip++
		return frame.Chunk.Code[frame.ip-1]
	}
	readConst := func() value.Value { return frame.Chunk.Constants[readByte()] }
	readShort := func() int {
		frame.ip += 2
		code := frame.Chunk.Code
		return int((uint(code[frame.ip-2]) << 8) | uint(code[frame.ip-1]))
	}

	for {
		// DEBUG_TRACE_EXEC
		TraceExecution(vm, frame.Chunk, frame.ip)

		switch inst := readByte(); opcode.OpCode(inst) {
		case opcode.OP_CONSTANT:
			val := readConst()
			vm.push(val)
		case opcode.OP_NIL:
			vm.push(value.Null{})
		case opcode.OP_FALSE:
			vm.push(value.Bool(false))
		case opcode.OP_TRUE:
			vm.push(value.Bool(true))
		case opcode.OP_POP:
			vm.pop()
		case opcode.OP_ARRAY:
			arrLen := int(readByte())
			startIdx := vm.sp - arrLen
			elements := make([]value.Value, vm.sp-startIdx)
			copy(elements, vm.stack[startIdx:vm.sp])
			vm.sp -= arrLen
			vm.push(value.NewObjArray(elements))
		case opcode.OP_HASH:
			mapLen := int(readByte() * 2)
			startIdx := vm.sp - mapLen
			pairs := make(map[value.Value]value.Value)
			for i := startIdx; i < vm.sp; i += 2 {
				key, val := vm.stack[i], vm.stack[i+1]
				if _, ok := key.(value.Hashable); !ok {
					return vm.runTimeErr("'%s' of type '%s' is an unhashable type", key, key.Type())
				}
				pairs[key] = val
			}
			vm.sp -= mapLen
			vm.push(value.NewObjMap(pairs))
		case opcode.OP_GET_GLOBAL:
			idx := int(readByte())
			vm.push(vm.Globals[idx])
		case opcode.OP_SET_GLOBAL:
			idx := int(readByte())
			vm.Globals[idx] = vm.peek(0)
		case opcode.OP_JUMP:
			offset := readShort()
			frame.ip += offset
		case opcode.OP_PRINT:
			PrintlnFn(vm.pop())
		case opcode.OP_RETURN:
			result := vm.pop()
			vm.closeUpvalues(frame.bp)
			vm.frameCnt--
			if vm.frameCnt == 0 {
				vm.pop()
				return nil
			}
			vm.sp = frame.bp
			vm.push(result)
			frame = &vm.frames[vm.frameCnt-1]
		default:
			panic(fmt.Sprintf("interpretation for '%s' is not implemented yet", opcode.OpCode(inst)))
		}
	}
}

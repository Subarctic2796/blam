package value

import (
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/opcode"
)

// stores the line info in rle form
type LineInfo struct {
	offset, line int
}

// this will be interpreted by the vm
// it has the instructions, constants
// and line info needed for everything
type Chunk struct {
	Code      []byte
	Constants []Value
	Lines     []LineInfo
}

func NewChunk() *Chunk {
	return &Chunk{
		make([]byte, 0),
		make([]Value, 0),
		make([]LineInfo, 0),
	}
}

// adds an instruction to the chunk
func (c *Chunk) WriteChunk(b byte, line int) {
	c.Code = append(c.Code, b)

	// rle encode line info
	if len(c.Lines) > 0 && c.Lines[len(c.Lines)-1].line == line {
		return
	}

	c.Lines = append(c.Lines, LineInfo{len(c.Code) - 1, line})
}

// adds a constant to a chunk
// returns that index so we can know where to
// find that constant later
func (c *Chunk) AddConst(value Value) int {
	c.Constants = append(c.Constants, value)
	return len(c.Constants) - 1
}

// used to get the line for an instruction
// in the rle encoded lines
func (c *Chunk) getLine(inst int) int {
	start, end, length := 0, len(c.Lines)-1, len(c.Lines)-1

	for {
		mid := (start + end) / 2
		line := c.Lines[mid]
		if inst < line.offset {
			end = mid - 1
		} else if mid == length || inst < c.Lines[mid+1].offset {
			return line.line
		} else {
			start = mid + 1
		}
	}
}

// disassembles a whole chunk
func DisassembleChunk(c *Chunk, name string) {
	fmt.Fprintf(os.Stderr, "== %s ==\n", name)
	for offset := 0; offset < len(c.Code); {
		offset = DisassembleInst(c, offset)
	}
}

// disassembles a single instruction
func DisassembleInst(c *Chunk, offset int) int {
	fmt.Fprintf(os.Stderr, "%04d ", offset)
	line := c.getLine(offset)
	if offset > 0 && line == c.getLine(offset-1) {
		fmt.Fprintf(os.Stderr, "   | ")
	} else {
		fmt.Fprintf(os.Stderr, "%4d ", line)
	}

	switch inst := opcode.OpCode(c.Code[offset]); inst {
	case opcode.OP_CLOSURE:
		const_ := c.Code[offset+1]
		offset += 2
		fn := c.Constants[const_].(*ObjFn)
		fmt.Fprintf(os.Stderr, "%-16s %4d %s\n", inst, const_, fn)
		scope := ""
		for range fn.UpvalueCnt {
			isLocal, idx := c.Code[offset], c.Code[offset+1]
			offset += 2
			if isLocal == 1 { // ie true
				scope = "local"
			} else {
				scope = "upvalue"
			}
			fmt.Fprintf(os.Stderr, "%04d      |                     %s %d\n", offset-2,
				scope, idx)
		}
		return offset
	case opcode.OP_INVOKE, opcode.OP_SUPER_INVOKE: // invokeInst
		idx, argc := c.Code[offset+1], c.Code[offset+2]
		val := c.Constants[idx]
		fmt.Fprintf(os.Stderr, "%-16s (%d args) %4d '%s'\n", inst, argc, idx, val)
		return offset + 3
	case opcode.OP_JUMP, opcode.OP_JUMP_IF_FALSE, opcode.OP_LOOP: // jumpInst
		jmp := int((uint(c.Code[offset+1]) << 8) | uint(c.Code[offset+2]))
		if inst == opcode.OP_LOOP {
			jmp = -jmp
		}
		fmt.Fprintf(os.Stderr, "%-16s %4d -> %d\n", inst, offset, offset+3+jmp)
		return offset + 3
	case opcode.OP_GET_LOCAL, opcode.OP_SET_LOCAL, // byteInst
		opcode.OP_GET_UPVALUE, opcode.OP_SET_UPVALUE,
		opcode.OP_ARRAY, opcode.OP_HASH,
		opcode.OP_CALL:
		slot := c.Code[offset+1]
		fmt.Fprintf(os.Stderr, "%-16s %4d\n", inst, slot)
		return offset + 2
	case opcode.OP_CONSTANT, // constantInst
		opcode.OP_DEFINE_GLOBAL,
		opcode.OP_GET_GLOBAL, opcode.OP_SET_GLOBAL,
		opcode.OP_GET_PROPERTY, opcode.OP_SET_PROPERTY,
		opcode.OP_GET_SUPER,
		opcode.OP_CLASS,
		opcode.OP_METHOD:
		idx := c.Code[offset+1]
		fmt.Fprintf(os.Stderr, "%-16s %4d '%s'\n", inst, idx, c.Constants[idx])
		return offset + 2
	case opcode.OP_NIL, opcode.OP_FALSE, opcode.OP_TRUE, // simpleInst
		opcode.OP_EQUAL, opcode.OP_NOT_EQUAL,
		opcode.OP_GREATER, opcode.OP_LESS,
		opcode.OP_GREATER_EQUAL, opcode.OP_LESS_EQUAL,
		opcode.OP_ADD, opcode.OP_SUBTRACT,
		opcode.OP_MULTIPLY, opcode.OP_DIVIDE,
		opcode.OP_NOT, opcode.OP_NEGATE,
		opcode.OP_GET_INDEX, opcode.OP_SET_INDEX,
		opcode.OP_PRINT,
		opcode.OP_POP,
		opcode.OP_INHERIT,
		opcode.OP_CLOSE_UPVALUE,
		opcode.OP_RETURN:
		fmt.Fprintln(os.Stderr, inst)
		return offset + 1
	default:
		fmt.Fprintf(os.Stderr, "Unknown opcode %d\n", inst)
		return offset + 1
	}
}

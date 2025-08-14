package compiler

import (
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/ast"
	"github.com/Subarctic2796/blam/opcodes"
	"github.com/Subarctic2796/blam/token"
	"github.com/Subarctic2796/blam/value"
)

const (
	_UINT16_MAX   = 65535
	_UINT8_MAX    = 255
	_UINT8_COUNT  = 256
	_MAX_LOCALS   = _UINT8_COUNT
	_MAX_UPVALUES = _UINT8_COUNT
)

type local struct {
	name   token.Token
	depth  int
	exitOP opcodes.OpCode
}

type upvalue struct {
	index   byte
	isLocal bool
}

type Compiler struct {
	enclosing      *Compiler
	fun            *value.ObjFn
	ftype          ast.FnType
	locals         [_MAX_LOCALS]local
	localCnt       int
	scopeDepth     int
	upvalues       []upvalue
	constantsTable map[value.Value]int
	lastOpCode     opcodes.OpCode
	curTok         *token.Token
	curErr         error
}

func NewCompiler(enclosing *Compiler, ft ast.FnType) *Compiler {
	c := &Compiler{
		enclosing:      enclosing,
		fun:            value.NewObjFn(),
		ftype:          ft,
		localCnt:       1,
		scopeDepth:     0,
		upvalues:       make([]upvalue, 0, _MAX_UPVALUES),
		constantsTable: make(map[value.Value]int),
		curTok:         nil,
		curErr:         nil,
	}

	if ft == ast.FN_SCRIPT {
		c.fun.Name = ""
	}

	if ft != ast.FN_FUNC {
		c.locals[0] = local{
			token.NewToken(token.THIS, "this", nil, -1),
			0,
			opcodes.OP_POP,
		}
	} else {
		c.locals[0] = local{
			token.NewToken(token.NUM_TOKENS, "", nil, -1),
			0,
			opcodes.OP_POP,
		}
	}

	return c
}

func (c *Compiler) Compile(prog []ast.Stmt, globals []value.Value) (*value.ObjFn, error) {
	for _, stmt := range prog {
		c.compileStmt(stmt)
	}

	return c.fun, c.curErr
}

func (c *Compiler) curChunk() *value.Chunk { return c.fun.Chunk }

func (c *Compiler) reportErr(msg string) {
	fmt.Fprintf(os.Stderr, "[line %d] Error", c.curTok.Line)
	switch c.curTok.Kind {
	case token.EOF:
		fmt.Fprint(os.Stderr, " at end")
	default:
		fmt.Fprintf(os.Stderr, " at '%s'", c.curTok.Lexeme)
	}
	fmt.Fprintf(os.Stderr, ": %s\n", msg)
	c.curErr = fmt.Errorf("%s", msg)
}

func (c *Compiler) emitBytes(args ...byte) {
	for _, b := range args {
		c.curChunk().WriteChunk(b, c.curTok.Line)
	}
}

func (c *Compiler) emitOp(op opcodes.OpCode) {
	c.emitBytes(byte(op))
	c.lastOpCode = op
}

func (c *Compiler) emitPOP() { c.emitOp(opcodes.OP_POP) }

func (c *Compiler) emitOpArgs(op opcodes.OpCode, args ...byte) {
	c.emitOp(op)
	c.emitBytes(args...)
}

func (c *Compiler) emitLoop(loopStart int) {
	c.emitOp(opcodes.OP_LOOP)

	offset := len(c.curChunk().Code) - loopStart + 2
	if offset > _UINT16_MAX {
		c.reportErr("Loop body too large")
	}

	c.emitBytes(byte((offset>>8)&0xff), byte(offset&0xff))
}

func (c *Compiler) emitJmp(op opcodes.OpCode) int {
	c.emitOpArgs(op, 0xff, 0xff)
	return len(c.curChunk().Code) - 2
}

func (c *Compiler) patchJmp(offset int) {
	jmp := len(c.curChunk().Code) - offset + 2

	if jmp > _UINT16_MAX {
		c.reportErr("Too much code to jump over")
	}

	c.curChunk().Code[offset] = byte((uint(jmp) >> 8) & 0xff)
	c.curChunk().Code[offset+1] = byte(jmp & 0xff)
}

func (c *Compiler) emitReturn() {
	if c.ftype == ast.FN_INIT {
		c.emitOpArgs(opcodes.OP_GET_LOCAL, 0)
	} else {
		c.emitOp(opcodes.OP_NIL)
	}
	c.emitOp(opcodes.OP_RETURN)
}

func (c *Compiler) mkConst(value value.Value) byte {
	if index, ok := c.constantsTable[value]; ok {
		// reuse the constant
		return byte(index)
	}
	idx := c.curChunk().AddConst(value)
	if idx > _UINT8_MAX {
		c.reportErr("Too many constants in one chunk")
		return 0
	}
	// constant doesn't exist so add it
	c.constantsTable[value] = idx
	return byte(idx)
}

func (c *Compiler) emitConst(value value.Value) {
	c.emitOpArgs(opcodes.OP_CONSTANT, c.mkConst(value))
}

func (c *Compiler) beginScope() { c.scopeDepth++ }
func (c *Compiler) endScope() {
	c.scopeDepth--
	for c.localCnt > 0 && c.locals[c.localCnt-1].depth > c.scopeDepth {
		c.emitOp(c.locals[c.localCnt-1].exitOP)
		c.localCnt--
	}
}

func (c *Compiler) addLocal(name token.Token) {
	if c.localCnt == _MAX_LOCALS {
		c.reportErr("Too many local variables in function")
		return
	}
	c.locals[c.localCnt] = local{name, -1, opcodes.OP_POP}
	c.localCnt++
}

func (c *Compiler) compileStmt(stmt ast.Stmt) {
	switch s := stmt.(type) {
	case *ast.BlockStmt:
		c.beginScope()
		for _, statement := range s.Statements {
			c.compileStmt(statement)
		}
		c.endScope()
	case *ast.ClassStmt:
		panic(fmt.Sprintf("compileStmt not implemented for '%T'", s))
	case *ast.ExprStmt:
		c.compileExpr(s.Expression)
		c.emitPOP()
	case *ast.FnStmt:
		panic(fmt.Sprintf("compileStmt not implemented for '%T'", s))
	case *ast.IfStmt:
		c.compileExpr(s.Cond)

		thenJmp := c.emitJmp(opcodes.OP_JUMP_IF_FALSE)
		c.emitPOP()
		c.compileStmt(s.ThenBranch)

		elseJmp := c.emitJmp(opcodes.OP_JUMP)

		c.patchJmp(thenJmp)
		c.emitPOP()

		if s.ElseBranch != nil {
			c.compileStmt(s.ElseBranch)
		}
		c.patchJmp(elseJmp)
	case *ast.PrintStmt:
		c.compileExpr(s.Expression)
		c.emitOp(opcodes.OP_PRINT)
	case *ast.ControlStmt:
		c.curTok = s.Keyword
		switch s.Keyword.Kind {
		case token.RETURN:
			if s.Value == nil {
				c.emitReturn()
			} else {
				c.compileExpr(s.Value)
				c.emitOp(opcodes.OP_RETURN)
			}
		case token.BREAK, token.CONTINUE:
			panic(fmt.Sprintf("compiling is not implemented for '%s'", s.Keyword))
		}
	case *ast.VarStmt:
		panic(fmt.Sprintf("compileStmt not implemented for '%T'", s))
	case *ast.WhileStmt:
		loopStart := len(c.curChunk().Code)
		c.compileExpr(s.Condition)

		exitJmp := c.emitJmp(opcodes.OP_JUMP_IF_FALSE)
		c.emitPOP()
		c.compileStmt(s.Body)
		c.emitLoop(loopStart)

		c.patchJmp(exitJmp)
		c.emitPOP()
	default:
		panic(fmt.Sprintf("compileStmt not implemented for '%T'", s))
	}
}

func (c *Compiler) compileExpr(expr ast.Expr) {
	switch e := expr.(type) {
	case *ast.ArrayLiteral:
		c.curTok = e.Sqr
		for _, el := range e.Elements {
			c.compileExpr(el)
		}
		c.emitOpArgs(opcodes.OP_ARRAY, byte(len(e.Elements)))
	case *ast.AssignExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.BinaryExpr:
		c.compileExpr(e.Left)
		c.curTok = e.Operator
		c.compileExpr(e.Right)
		switch e.Operator.Kind {
		case token.NEQ:
			c.emitOp(opcodes.OP_NOT_EQUAL)
		case token.EQ_EQ:
			c.emitOp(opcodes.OP_EQUAL)
		case token.GT:
			c.emitOp(opcodes.OP_GREATER)
		case token.GT_EQ:
			c.emitOp(opcodes.OP_GREATER_EQUAL)
		case token.LT:
			c.emitOp(opcodes.OP_LESS)
		case token.LT_EQ:
			c.emitOp(opcodes.OP_LESS_EQUAL)
		case token.PLUS:
			c.emitOp(opcodes.OP_ADD)
		case token.MINUS:
			c.emitOp(opcodes.OP_SUBTRACT)
		case token.STAR:
			c.emitOp(opcodes.OP_MULTIPLY)
		case token.SLASH:
			c.emitOp(opcodes.OP_DIVIDE)
		default:
			return // unreachable
		}
	case *ast.CallExpr:
		c.compileExpr(e.Callee)
		c.curTok = e.Paren
		for _, arg := range e.Arguments {
			c.compileExpr(arg)
		}
		c.emitOpArgs(opcodes.OP_CALL, byte(len(e.Arguments)))
	case *ast.IndexedGetExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.GroupingExpr:
		c.compileExpr(e.Expression)
	case *ast.IfExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.GetExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.HashLiteral:
		c.curTok = e.Brace
		for k, v := range e.Pairs {
			c.compileExpr(k)
			c.compileExpr(v)
		}
		c.emitOpArgs(opcodes.OP_HASH, byte(len(e.Pairs)))
	case *ast.LambdaExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.Literal:
		switch v := e.Value.(type) {
		case float64:
			c.emitConst(value.Num(v))
		case string:
			c.emitConst(value.String(v))
		case bool:
			if v {
				c.emitOp(opcodes.OP_TRUE)
			}
			c.emitOp(opcodes.OP_FALSE)
		case nil:
			c.emitOp(opcodes.OP_NIL)
		}
	case *ast.LogicalExpr:
		c.compileExpr(e.Left)

		switch e.Operator.Kind {
		case token.AND:
			endJmp := c.emitJmp(opcodes.OP_JUMP_IF_FALSE)
			c.emitPOP()
			c.curTok = e.Operator
			c.compileExpr(e.Right)
			c.patchJmp(endJmp)
		case token.OR:
			elseJmp := c.emitJmp(opcodes.OP_JUMP_IF_FALSE)
			endJmp := c.emitJmp(opcodes.OP_JUMP)

			c.patchJmp(elseJmp)
			c.emitPOP()

			c.curTok = e.Operator
			c.compileExpr(e.Right)
			c.patchJmp(endJmp)
		}
	case *ast.SetExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.IndexedSetExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.SuperExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.ThisExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.UnaryExpr:
		c.curTok = e.Operator
		c.compileExpr(e.Right)
		switch e.Operator.Kind { // emit the opr inst
		case token.BANG:
			c.emitOp(opcodes.OP_NOT)
		case token.MINUS:
			c.emitOp(opcodes.OP_NEGATE)
		default:
			return // unreachable
		}
	case *ast.VariableExpr:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	default:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	}
}

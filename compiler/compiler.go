package compiler

import (
	"fmt"
	"os"

	"github.com/Subarctic2796/blam/ast"
	"github.com/Subarctic2796/blam/opcode"
	"github.com/Subarctic2796/blam/token"
	"github.com/Subarctic2796/blam/value"
)

const (
	_UINT16_MAX   = 65535
	_UINT8_MAX    = 255
	_UINT8_COUNT  = 256
	_MAX_LOCALS   = _UINT8_COUNT
	_MAX_UPVALUES = _UINT8_COUNT

	isLocalFalse byte = 0
	isLocalTrue  byte = 1
)

type local struct {
	name   token.Token
	depth  int
	exitOP opcode.OpCode
}

type upvalue struct {
	index   byte
	isLocal byte
}

type symbol struct {
	scope ast.Scope
	index int
}

type classCompiler struct {
	enclosing     *classCompiler
	hasSuperclass bool

	// name to index in fields slice
	fields map[string]int
	// name to index in methods slice
	methods map[string]int
}

func newClassCompiler(enclosing *classCompiler) *classCompiler {
	return &classCompiler{
		enclosing,
		false,
		make(map[string]int),
		make(map[string]int),
	}
}

type Compiler struct {
	enclosing      *Compiler
	enclosingClass *classCompiler
	fun            *value.ObjFn
	ftype          ast.FnType
	locals         [_MAX_LOCALS]local
	localCnt       int
	scopeDepth     int
	upvalues       []upvalue
	constantTable  map[value.Value]int
	globals        []value.Value
	globalsTable   map[string]int
	lastOpCode     opcode.OpCode
	tok            *token.Token
	curErr         error
}

func NewCompiler(enclosing *Compiler, ft ast.FnType, globals []value.Value, globalsTable map[string]int) *Compiler {
	var tok *token.Token
	var curErr error
	if enclosing != nil {
		tok = enclosing.tok
		curErr = enclosing.curErr
	}
	c := &Compiler{
		enclosing:      enclosing,
		enclosingClass: nil,
		fun:            value.NewObjFn(),
		ftype:          ft,
		localCnt:       1,
		scopeDepth:     0,
		upvalues:       make([]upvalue, 0, _MAX_UPVALUES),
		constantTable:  make(map[value.Value]int),
		globals:        globals,
		globalsTable:   globalsTable,
		tok:            tok,
		curErr:         curErr,
	}

	// the script doesn't have a name
	if ft == ast.FN_SCRIPT {
		c.fun.Name = ""
	}

	// the first local is either this if it is a method
	// or it is the function itself
	if ft != ast.FN_FUNC {
		c.locals[0] = local{
			token.NewToken(token.THIS, "this", nil, -1),
			0,
			opcode.OP_POP,
		}
	} else {
		c.locals[0] = local{
			token.NewToken(token.NUM_TOKENS, "", nil, -1),
			0,
			opcode.OP_POP,
		}
	}

	return c
}

// this finishes off the currently being compiled function
// everything is wrapped in a function called '<script>'
// this can be thought of as a main function
func (c *Compiler) endCompiler() (*Compiler, *value.ObjFn) {
	if c.lastOpCode != opcode.OP_RETURN {
		c.emitReturn()
	}

	fn := c.fun
	if c.curErr == nil {
		// DEBUG_PRINT_CODE
		if c.fun.Name == "" {
			value.DisassembleChunk(c.curChunk(), "<script>")
		} else {
			value.DisassembleChunk(c.curChunk(), string(c.fun.Name))
		}
	}

	return c.enclosing, fn
}

// compiles the program
func (c *Compiler) Compile(prog []ast.Stmt) (*value.ObjFn, error) {
	for _, stmt := range prog {
		c.compileStmt(stmt)
	}
	_, fn := c.endCompiler()
	return fn, c.curErr
}

func (c *Compiler) curChunk() *value.Chunk { return c.fun.Chunk }

func (c *Compiler) reportErr(msg string) {
	switch c.tok.Kind {
	case token.EOF:
		fmt.Fprintf(os.Stderr, "[line %d] Error at end: %s\n", c.tok.Line, msg)
	default:
		fmt.Fprintf(os.Stderr, "[line %d] Error at '%s': %s\n", c.tok.Line, c.tok.Lexeme, msg)
	}
	c.curErr = fmt.Errorf("%s", msg)
}

func (c *Compiler) emitBytes(args ...byte) {
	for _, b := range args {
		c.curChunk().WriteChunk(b, c.tok.Line)
	}
}

func (c *Compiler) emitOp(op opcode.OpCode) {
	c.emitBytes(byte(op))
	c.lastOpCode = op
}

func (c *Compiler) emitPOP() { c.emitOp(opcode.OP_POP) }

func (c *Compiler) emitOpArgs(op opcode.OpCode, args ...byte) {
	c.emitOp(op)
	c.emitBytes(args...)
}

func (c *Compiler) emitLoop(loopStart int) {
	c.emitOp(opcode.OP_LOOP)

	offset := len(c.curChunk().Code) - loopStart + 2
	if offset > _UINT16_MAX {
		c.reportErr("Loop body too large")
	}

	c.emitBytes(byte((offset>>8)&0xff), byte(offset&0xff))
}

func (c *Compiler) emitJmp(op opcode.OpCode) int {
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
		c.emitOpArgs(opcode.OP_GET_LOCAL, 0)
	} else {
		c.emitOp(opcode.OP_NIL)
	}
	c.emitOp(opcode.OP_RETURN)
}

func (c *Compiler) mkConst(value value.Value) byte {
	if index, ok := c.constantTable[value]; ok {
		// reuse the constant
		return byte(index)
	}
	idx := c.curChunk().AddConst(value)
	if idx > _UINT8_MAX {
		c.reportErr("Too many constants in one chunk")
		return 0
	}
	// constant doesn't exist so add it
	c.constantTable[value] = idx
	return byte(idx)
}

func (c *Compiler) emitConst(value value.Value) {
	c.emitOpArgs(opcode.OP_CONSTANT, c.mkConst(value))
}

func (c *Compiler) beginScope() { c.scopeDepth++ }
func (c *Compiler) endScope() {
	c.scopeDepth--
	for c.localCnt > 0 && c.locals[c.localCnt-1].depth > c.scopeDepth {
		c.emitOp(c.locals[c.localCnt-1].exitOP)
		c.localCnt--
	}
}

func (c *Compiler) addLocal(name token.Token) int {
	if c.localCnt == _MAX_LOCALS {
		c.reportErr("Too many local variables in function")
		return -1
	}
	c.locals[c.localCnt] = local{name, c.scopeDepth, opcode.OP_POP}
	c.localCnt++
	return c.localCnt - 1
}

func (c *Compiler) addUpvalue(isLocal byte, index int) int {
	cnt := c.fun.UpvalueCnt
	for ix, upv := range c.upvalues {
		if upv.index == byte(index) && upv.isLocal == isLocal {
			return ix
		}
	}

	if cnt == _MAX_UPVALUES {
		c.reportErr("Too many upvalues for the scope")
		return -1
	}
	c.upvalues = append(c.upvalues, upvalue{byte(index), isLocal})
	c.fun.UpvalueCnt++
	return cnt
}

func (c *Compiler) declareVar(name *token.Token) int {
	if c.scopeDepth == 0 {
		if idx, ok := c.globalsTable[name.Lexeme]; ok {
			// already got this variable
			return idx
		}
		// need to add it
		c.globals = append(c.globals, value.Null{})
		c.globalsTable[name.Lexeme] = len(c.globals) - 1
		return len(c.globals) - 1
	}
	return c.addLocal(*name)
}

func (c *Compiler) defineVar(symbol int) {
	// panic("TODO_: defineVar")
	if c.scopeDepth >= 0 {
		return
	}
	c.emitOpArgs(opcode.OP_SET_GLOBAL, byte(symbol))
	c.emitPOP()
}

func (c *Compiler) resolveLocal(name token.Token) int {
	for i := c.localCnt - 1; i >= 0; i-- {
		if c.locals[i].name.Lexeme == name.Lexeme {
			return i
		}
	}
	return -1
}

func (c *Compiler) findUpvalue(name *token.Token) int {
	if c.enclosing == nil {
		return -1
	}

	if idx := c.enclosing.resolveLocal(*name); idx != -1 {
		c.enclosing.locals[idx].exitOP = opcode.OP_CLOSE_UPVALUE
		return c.addUpvalue(isLocalTrue, idx)
	}

	if idx := c.enclosing.findUpvalue(name); idx != -1 {
		return c.addUpvalue(isLocalFalse, idx)
	}

	return -1
}

func (c *Compiler) resolveNonmodule(name *token.Token) symbol {
	symbol := symbol{ast.SCOPE_LOCAL, -1}
	symbol.index = c.resolveLocal(*name)
	if symbol.index != -1 {
		return symbol
	}
	symbol.scope = ast.SCOPE_UPVALUE
	symbol.index = c.findUpvalue(name)
	return symbol
}

func (c *Compiler) loadVariable(symbol symbol) {
	switch symbol.scope {
	case ast.SCOPE_GLOBAL:
		c.emitOpArgs(opcode.OP_GET_GLOBAL, byte(symbol.index))
	case ast.SCOPE_LOCAL:
		c.emitOpArgs(opcode.OP_GET_LOCAL, byte(symbol.index))
	case ast.SCOPE_UPVALUE:
		c.emitOpArgs(opcode.OP_GET_UPVALUE, byte(symbol.index))
	default:
		panic("unreachable")
	}
}

func (c *Compiler) bareName(symbol symbol, expr ast.Expr) {
	if expr != nil {
		c.compileExpr(expr)
		switch symbol.scope {
		case ast.SCOPE_LOCAL:
			c.emitOpArgs(opcode.OP_SET_LOCAL, byte(symbol.index))
		case ast.SCOPE_UPVALUE:
			c.emitOpArgs(opcode.OP_SET_UPVALUE, byte(symbol.index))
		case ast.SCOPE_GLOBAL:
			c.emitOpArgs(opcode.OP_SET_GLOBAL, byte(symbol.index))
		}
		return
	}
	c.loadVariable(symbol)
}

func (c *Compiler) compileFunc(stmt *ast.FnStmt) {
	compiler := NewCompiler(c, stmt.Kind, c.globals, c.globalsTable)
	c = compiler
	c.beginScope()
	c.fun.Name = value.String(stmt.Name.Lexeme)
	c.fun.Arity = len(stmt.Params)

	for _, param := range stmt.Params {
		const_ := c.declareVar(param)
		c.defineVar(const_)
	}

	// compile body
	for _, s := range stmt.Body {
		c.compileStmt(s)
	}

	enclosing, fun := c.endCompiler()
	c = enclosing
	c.emitOpArgs(opcode.OP_CLOSURE, c.mkConst(fun))

	for _, upv := range c.upvalues {
		c.emitBytes(upv.isLocal, upv.index)
	}
}

func (c *Compiler) compileStmt(stmt ast.Stmt) {
	switch s := stmt.(type) {
	case *ast.BlockStmt:
		c.tok = s.Brace
		c.beginScope()
		for _, statement := range s.Statements {
			c.compileStmt(statement)
		}
		c.endScope()
	case *ast.ClassStmt:
		c.tok = s.Name
		panic(fmt.Sprintf("compileStmt not implemented for '%T'", s))
	case *ast.ExprStmt:
		c.tok = s.Token
		c.compileExpr(s.Expression)
		c.emitPOP()
	case *ast.FnStmt:
		c.tok = s.Name
		symbol := c.declareVar(s.Name)
		c.compileFunc(s)
		c.defineVar(symbol)
	case *ast.IfStmt:
		c.tok = s.Keyword
		c.compileExpr(s.Cond)

		thenJmp := c.emitJmp(opcode.OP_JUMP_IF_FALSE)
		c.emitPOP()
		c.compileStmt(s.ThenBranch)

		elseJmp := c.emitJmp(opcode.OP_JUMP)

		c.patchJmp(thenJmp)
		c.emitPOP()

		if s.ElseBranch != nil {
			c.compileStmt(s.ElseBranch)
		}
		c.patchJmp(elseJmp)
	case *ast.PrintStmt:
		c.tok = s.Keyword
		c.compileExpr(s.Expression)
		c.emitOp(opcode.OP_PRINT)
	case *ast.ControlStmt:
		c.tok = s.Keyword
		switch s.Keyword.Kind {
		case token.RETURN:
			if s.Value == nil {
				c.emitReturn()
			} else {
				c.compileExpr(s.Value)
				c.emitOp(opcode.OP_RETURN)
			}
		case token.BREAK, token.CONTINUE:
			panic(fmt.Sprintf("compiling is not implemented for '%s'", s.Keyword))
		}
	case *ast.VarStmt:
		c.tok = s.Name
		if s.Initializer == nil {
			c.emitOp(opcode.OP_NIL)
		} else {
			c.compileExpr(s.Initializer)
		}
		symbolIndex := c.declareVar(s.Name)
		c.defineVar(symbolIndex)
	case *ast.WhileStmt:
		c.tok = s.Keyword
		loopStart := len(c.curChunk().Code)
		c.compileExpr(s.Condition)

		exitJmp := c.emitJmp(opcode.OP_JUMP_IF_FALSE)
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
		c.tok = e.Sqr
		for _, el := range e.Elements {
			c.compileExpr(el)
		}
		c.emitOpArgs(opcode.OP_ARRAY, byte(len(e.Elements)))
	case *ast.AssignExpr:
		c.tok = e.Name
		symbol := c.resolveNonmodule(e.Name)
		if symbol.index != -1 {
			// locals or upvalues
			c.bareName(symbol, e.Value)
			return
		}
		// globals
		symbol.scope = ast.SCOPE_GLOBAL
		idx, ok := c.globalsTable[e.Name.Lexeme]
		if !ok {
			c.globals = append(c.globals, value.Null{})
			symbol.index = len(c.globals) - 1
		} else {
			symbol.index = idx
		}
		c.bareName(symbol, e.Value)
		// panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.BinaryExpr:
		c.compileExpr(e.Left)
		c.tok = e.Operator
		c.compileExpr(e.Right)
		switch e.Operator.Kind {
		case token.NEQ:
			c.emitOp(opcode.OP_NOT_EQUAL)
		case token.EQ_EQ:
			c.emitOp(opcode.OP_EQUAL)
		case token.GT:
			c.emitOp(opcode.OP_GREATER)
		case token.GT_EQ:
			c.emitOp(opcode.OP_GREATER_EQUAL)
		case token.LT:
			c.emitOp(opcode.OP_LESS)
		case token.LT_EQ:
			c.emitOp(opcode.OP_LESS_EQUAL)
		case token.PLUS:
			c.emitOp(opcode.OP_ADD)
		case token.MINUS:
			c.emitOp(opcode.OP_SUBTRACT)
		case token.STAR:
			c.emitOp(opcode.OP_MULTIPLY)
		case token.SLASH:
			c.emitOp(opcode.OP_DIVIDE)
		default:
			return // unreachable
		}
	case *ast.CallExpr:
		c.compileExpr(e.Callee)
		c.tok = e.Paren
		for _, arg := range e.Arguments {
			c.compileExpr(arg)
		}
		c.emitOpArgs(opcode.OP_CALL, byte(len(e.Arguments)))
	case *ast.IndexedGetExpr:
		c.compileExpr(e.Object)
		c.tok = e.Sqr
		if e.Colon != nil || e.Start == nil {
			panic("compiling for slicing is not implemented yet")
		}
		c.compileExpr(e.Start)
		c.emitOp(opcode.OP_GET_INDEX)
	case *ast.GroupingExpr:
		c.compileExpr(e.Expression)
	case *ast.IfExpr:
		c.tok = e.If.Keyword
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.GetExpr:
		c.tok = e.Name
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.HashLiteral:
		c.tok = e.Brace
		for k, v := range e.Pairs {
			c.compileExpr(k)
			c.compileExpr(v)
		}
		c.emitOpArgs(opcode.OP_HASH, byte(len(e.Pairs)))
	case *ast.LambdaExpr:
		c.tok = e.Name
		c.compileFunc(e.FnStmt)
	case *ast.Literal:
		switch v := e.Value.(type) {
		case float64:
			c.emitConst(value.Num(v))
		case string:
			c.emitConst(value.String(v))
		case bool:
			if v {
				c.emitOp(opcode.OP_TRUE)
			} else {
				// else to make sure that an OP_FALSE isn't added
				// to the bytecode
				c.emitOp(opcode.OP_FALSE)
			}
		case nil:
			c.emitOp(opcode.OP_NIL)
		}
	case *ast.LogicalExpr:
		c.compileExpr(e.Left)

		switch e.Operator.Kind {
		case token.AND:
			endJmp := c.emitJmp(opcode.OP_JUMP_IF_FALSE)
			c.emitPOP()
			c.tok = e.Operator
			c.compileExpr(e.Right)
			c.patchJmp(endJmp)
		case token.OR:
			elseJmp := c.emitJmp(opcode.OP_JUMP_IF_FALSE)
			endJmp := c.emitJmp(opcode.OP_JUMP)

			c.patchJmp(elseJmp)
			c.emitPOP()

			c.tok = e.Operator
			c.compileExpr(e.Right)
			c.patchJmp(endJmp)
		}
	case *ast.SetExpr:
		c.tok = e.Name
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.IndexedSetExpr:
		c.compileExpr(e.Object)
		c.tok = e.Sqr

		c.compileExpr(e.Index)

		c.compileExpr(e.Value)
		c.emitOp(opcode.OP_SET_INDEX)
	case *ast.SuperExpr:
		c.tok = e.Keyword
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.ThisExpr:
		c.tok = e.Keyword
		// c.loadVariable(c.resolveNonmodule(e.Keyword))
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	case *ast.UnaryExpr:
		c.tok = e.Operator
		c.compileExpr(e.Right)
		switch e.Operator.Kind { // emit the opr inst
		case token.BANG:
			c.emitOp(opcode.OP_NOT)
		case token.MINUS:
			c.emitOp(opcode.OP_NEGATE)
		default:
			return // unreachable
		}
	case *ast.IdentExpr:
		c.tok = e.Name
		symbol := c.resolveNonmodule(e.Name)
		if symbol.index != -1 {
			// locals or upvalues
			c.bareName(symbol, nil)
			return
		}
		// globals
		symbol.scope = ast.SCOPE_GLOBAL
		idx, ok := c.globalsTable[e.Name.Lexeme]
		if !ok {
			c.globals = append(c.globals, value.Null{})
			symbol.index = len(c.globals) - 1
		} else {
			symbol.index = idx
		}
		c.bareName(symbol, nil)
	default:
		panic(fmt.Sprintf("compileExpr not implemented for '%T'", e))
	}
}

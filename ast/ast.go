package ast

import (
	"fmt"
	"strings"

	"github.com/Subarctic2796/blam/token"
)

//go:generate go tool stringer -type=FnType,Scope -output=ast_strings.go -trimprefix=FN_
type FnType byte

const (
	FN_NONE FnType = iota
	FN_SCRIPT
	FN_NATIVE
	FN_LAMBDA
	FN_FUNC
	FN_INIT
	FN_METHOD
	FN_STATIC
)

type Scope byte

const (
	SCOPE_GLOBAL     Scope = iota
	SCOPE_NOT_GLOBAL       // either upvalue or local needs to be resovled
	SCOPE_LOCAL
	SCOPE_UPVALUE
)

type ScopeInfo struct {
	Scope Scope
	Depth int
	Index int
}

type Expr interface {
	String() string
}

type ArrayLiteral struct {
	Sqr      *token.Token
	Elements []Expr
}

func (e *ArrayLiteral) String() string {
	var sb strings.Builder
	sb.WriteString("([\n")
	for _, elm := range e.Elements {
		sb.WriteString(fmt.Sprintf("    %s,\n", elm))
	}
	sb.WriteString("])")
	return sb.String()
}

type AssignExpr struct {
	Name  *token.Token
	Value Expr
}

func (e *AssignExpr) String() string {
	return fmt.Sprintf("(= %s %s)", e.Name.Lexeme, e.Value)
}

type BinaryExpr struct {
	Left     Expr
	Operator *token.Token
	Right    Expr
}

func (e *BinaryExpr) String() string {
	return fmt.Sprintf("(%s %s %s)", e.Operator.Lexeme, e.Left, e.Right)
}

type CallExpr struct {
	Callee    Expr
	Paren     *token.Token
	Arguments []Expr
}

func (e *CallExpr) String() string {
	var sb strings.Builder
	for _, arg := range e.Arguments {
		sb.WriteString(" ")
		sb.WriteString(arg.String())
	}
	return fmt.Sprintf("(call %s%s)", e.Callee, sb.String())
}

type IndexedGetExpr struct {
	Object Expr
	Sqr    *token.Token
	Start  Expr
	Colon  *token.Token
	Stop   Expr
}

func (e *IndexedGetExpr) String() string {
	if e.Stop != nil {
		return fmt.Sprintf("(%s[%s:%s])", e.Object, e.Start, e.Stop)
	}
	return fmt.Sprintf("(%s[%s])", e.Object, e.Start)
}

type GroupingExpr struct {
	Expression Expr
}

func (e *GroupingExpr) String() string {
	return fmt.Sprintf("(group %s)", e.Expression)
}

type IfExpr struct {
	If *IfStmt
}

func (e *IfExpr) String() string { return e.If.String() }

type GetExpr struct {
	Object Expr
	Name   *token.Token
}

func (e *GetExpr) String() string {
	return fmt.Sprintf("(. %s %s)", e.Object, e.Name.Lexeme)
}

type HashLiteral struct {
	Brace *token.Token
	Pairs map[Expr]Expr
}

func (e *HashLiteral) String() string {
	var sb strings.Builder
	sb.WriteString("({\n")
	for k, v := range e.Pairs {
		sb.WriteString(fmt.Sprintf("    %s: %s,\n", k, v))
	}
	sb.WriteString("})")
	return sb.String()
}

type LambdaExpr struct {
	// use composition, this is like inheritance in go
	*FnStmt
}

func (e *LambdaExpr) String() string { return e.FnStmt.String() }

type Literal struct {
	Value any
}

func (e *Literal) String() string {
	if e.Value == nil {
		return "nil"
	}
	return fmt.Sprint(e.Value)
}

type LogicalExpr struct {
	Left     Expr
	Operator *token.Token
	Right    Expr
}

func (e *LogicalExpr) String() string {
	return fmt.Sprintf("(%s %s %s)", e.Operator.Lexeme, e.Left, e.Right)
}

type SetExpr struct {
	Object Expr
	Name   *token.Token
	Value  Expr
}

func (e *SetExpr) String() string {
	return fmt.Sprintf("(= %s %s %s)", e.Object, e.Name.Lexeme, e.Value)
}

type IndexedSetExpr struct {
	Object Expr
	Sqr    *token.Token
	Index  Expr
	Value  Expr
}

func (e *IndexedSetExpr) String() string {
	return fmt.Sprintf("(= %s[%s] %s)", e.Object, e.Index, e.Value)
}

type SuperExpr struct {
	Keyword *token.Token
	Method  *token.Token
}

func (e *SuperExpr) String() string {
	return fmt.Sprintf("(super %s)", e.Keyword.Lexeme)
}

type ThisExpr struct {
	Keyword    *token.Token
	ScopeDepth int
}

func (e *ThisExpr) String() string { return "(this)" }

type UnaryExpr struct {
	Operator *token.Token
	Right    Expr
}

func (e *UnaryExpr) String() string {
	return fmt.Sprintf("(%s %s)", e.Operator.Lexeme, e.Right)
}

type IdentExpr struct {
	Name *token.Token
}

func (e *IdentExpr) String() string { return e.Name.Lexeme }

type Stmt interface {
	String() string
}

type BlockStmt struct {
	Brace      *token.Token
	Statements []Stmt
}

func (s *BlockStmt) String() string {
	var sb strings.Builder
	sb.WriteString("(block ")
	for _, stmt := range s.Statements {
		sb.WriteString(stmt.String())
	}
	sb.WriteByte(')')
	return sb.String()
}

type ClassStmt struct {
	Name       *token.Token
	Superclass *IdentExpr
	Methods    []*FnStmt
}

func (s *ClassStmt) String() string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("(class %s", s.Name.Lexeme))
	if s.Superclass != nil {
		sb.WriteString(" < ")
		sb.WriteString(s.Superclass.String())
	}
	for _, fn := range s.Methods {
		sb.WriteString(" ")
		sb.WriteString(fn.String())
	}
	sb.WriteByte(')')
	return sb.String()
}

type ExprStmt struct {
	Token      *token.Token // first token of the expression
	Expression Expr
}

func (s *ExprStmt) String() string { return fmt.Sprintf("(; %s)", s.Expression) }

type FnStmt struct {
	Name   *token.Token
	Params []*token.Token
	Body   []Stmt
	Kind   FnType
}

func (s *FnStmt) String() string {
	var sb strings.Builder
	if s.Kind == FN_LAMBDA {
		sb.WriteString("(fun(")
	} else {
		sb.WriteString(fmt.Sprintf("(fun %s(", s.Name.Lexeme))
	}
	for _, param := range s.Params {
		if param != s.Params[0] {
			sb.WriteByte(' ')
		}
		sb.WriteString(param.Lexeme)
	}
	sb.WriteString(") ")
	for _, stmt := range s.Body {
		sb.WriteString(stmt.String())

	}
	sb.WriteByte(')')
	return sb.String()
}

type IfStmt struct {
	Keyword    *token.Token // the 'if' Keyword
	Cond       Expr
	ThenBranch Stmt
	ElseBranch Stmt
}

func (s *IfStmt) String() string {
	if s.ElseBranch == nil {
		return fmt.Sprintf("(if %s %s)", s.Cond, s.ThenBranch)
	}
	return fmt.Sprintf("(if-else %s %s %s)", s.Cond, s.ThenBranch, s.ElseBranch)
}

type PrintStmt struct {
	Keyword    *token.Token
	Expression Expr
}

func (s *PrintStmt) String() string { return fmt.Sprintf("(print %s)", s.Expression) }

type ControlStmt struct {
	Keyword *token.Token
	Value   Expr
}

func (s *ControlStmt) String() string {
	switch s.Keyword.Kind {
	case token.RETURN:
		if s.Value == nil {
			return "(return)"
		}
		return fmt.Sprintf("(return %s)", s.Value)
	case token.BREAK:
		return "(break)"
	case token.CONTINUE:
		return "(continue)"
	default:
		panic("unreachable")
	}
}

type VarStmt struct {
	Name        *token.Token
	Initializer Expr
}

func (s *VarStmt) String() string {
	if s.Initializer == nil {
		return fmt.Sprintf("(var %s)", s.Name.Lexeme)
	}
	return fmt.Sprintf("(var %s = %s)", s.Name.Lexeme, s.Initializer)
}

type WhileStmt struct {
	Keyword   *token.Token // the 'while' keyword
	Condition Expr
	Body      Stmt
}

func (s *WhileStmt) String() string {
	return fmt.Sprintf("(while %s %s)", s.Condition, s.Body)
}

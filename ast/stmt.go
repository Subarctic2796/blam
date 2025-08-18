package ast

import (
	"fmt"
	"strings"

	"github.com/Subarctic2796/blam/token"
)

//go:generate go tool stringer -type=FnType -output=common_strings.go -trimprefix=FN_
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
	Superclass *VariableExpr
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

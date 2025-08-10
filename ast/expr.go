package ast

import (
	"fmt"
	"strings"

	"github.com/Subarctic2796/blam/token"
)

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
	Name       *token.Token
	Value      Expr
	ScopeDepth int
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
	Func *FnStmt
}

func (e *LambdaExpr) String() string { return e.Func.String() }

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
	Keyword    *token.Token
	Method     *token.Token
	ScopeDepth int
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

type VariableExpr struct {
	Name       *token.Token
	ScopeDepth int
}

func (e *VariableExpr) String() string { return e.Name.Lexeme }

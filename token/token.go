package token

import "fmt"

//go:generate go tool stringer -type=TokenType
type TokenType byte // use byte as there are currently only 44 types of token

type Token struct {
	Kind    TokenType
	Lexeme  string
	Literal any
	Line    int
}

func NewToken(kind TokenType, lexeme string, literal any, line int) Token {
	return Token{kind, lexeme, literal, line}
}

func (t Token) String() string {
	return fmt.Sprintf("%s: '%s' %v %d", t.Kind, t.Lexeme, t.Literal, t.Line)
}

const (
	LPAREN TokenType = iota
	RPAREN
	LBRACE
	RBRACE
	LSQR
	RSQR
	COMMA
	DOT
	MINUS
	PLUS
	SEMICOLON
	COLON
	SLASH
	STAR
	// One or two character tokens.
	BANG
	NEQ
	EQ
	EQ_EQ
	GT
	GT_EQ
	LT
	LT_EQ
	// Literals.
	IDENTIFIER
	STRING
	NUMBER
	// Keywords.
	AND
	BREAK
	CLASS
	CONTINUE
	ELSE
	FALSE
	FOR
	FUN
	IF
	NIL
	OR
	PRINT // NOTE: temporary until have built in functions
	RETURN
	SUPER
	THIS
	TRUE
	VAR
	WHILE

	EOF

	NUM_TOKENS
)

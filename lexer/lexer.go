package lexer

import (
	"errors"
	"fmt"
	"os"
	"strconv"

	"github.com/Subarctic2796/blam/token"
)

var keywords = map[string]token.TokenType{
	"and":      token.AND,
	"class":    token.CLASS,
	"else":     token.ELSE,
	"false":    token.FALSE,
	"for":      token.FOR,
	"fun":      token.FUN,
	"if":       token.IF,
	"nil":      token.NIL,
	"or":       token.OR,
	"print":    token.PRINT,
	"return":   token.RETURN,
	"super":    token.SUPER,
	"this":     token.THIS,
	"true":     token.TRUE,
	"var":      token.VAR,
	"while":    token.WHILE,
	"break":    token.BREAK,
	"continue": token.CONTINUE,
}

var (
	ErrUnexpectedChar      = errors.New("unexpected character")
	ErrUnterminatedStr     = errors.New("unterminated string")
	ErrUnterminatedComment = errors.New("unterminated comment")
)

type Lexer struct {
	src              []rune
	start, cur, Line int
	tokens           []token.Token
	curErr           error
}

func NewLexer(src string) *Lexer {
	return &Lexer{[]rune(src), 0, 0, 1, make([]token.Token, 0), nil}
}

func (l *Lexer) ScanTokens() ([]token.Token, error) {
	for !l.isAtEnd() {
		l.start = l.cur
		l.scanToken()
	}
	l.tokens = append(l.tokens, token.NewToken(token.EOF, "", nil, l.Line))
	return l.tokens, l.curErr
}

func (l *Lexer) scanToken() {
	switch c := l.advance(); c {
	case '(':
		l.addToken(token.LPAREN)
	case ')':
		l.addToken(token.RPAREN)
	case '{':
		l.addToken(token.LBRACE)
	case '}':
		l.addToken(token.RBRACE)
	case '[':
		l.addToken(token.LSQR)
	case ']':
		l.addToken(token.RSQR)
	case ',':
		l.addToken(token.COMMA)
	case '.':
		l.addToken(token.DOT)
	case ';':
		l.addToken(token.SEMICOLON)
	case ':':
		l.addToken(token.COLON)
	case '*':
		l.addToken(token.STAR)
	case '+':
		l.addToken(token.PLUS)
	case '-':
		l.addToken(token.MINUS)
	case '!':
		l.addMatchToken('=', token.NEQ, token.BANG)
	case '=':
		l.addMatchToken('=', token.EQ_EQ, token.EQ)
	case '<':
		l.addMatchToken('=', token.LT_EQ, token.LT)
	case '>':
		l.addMatchToken('=', token.GT_EQ, token.GT)
	case '/':
		if l.match('/') {
			for l.peek() != '\n' && !l.isAtEnd() {
				l.advance()
			}
		} else if l.match('*') {
			l.multiLineComment()
		} else {
			l.addToken(token.SLASH)
		}
	case ' ', '\r', '\t':
	case '\n':
		l.Line++
	case '"':
		l.addString()
	default:
		if isDigit(c) {
			l.addNumber()
		} else if isAlpha(c) {
			l.identifier()
		} else {
			l.report(ErrUnexpectedChar)
		}
	}
}

func (l *Lexer) multiLineComment() {
	nesting := 1
	for nesting > 0 && !l.isAtEnd() {
		p, pn := l.peek(), l.peekNext()
		if p == '\n' || l.src[l.cur] == '\n' {
			l.Line++
		}
		if p == '/' && pn == '*' {
			l.advance()
			l.advance()
			nesting++
			continue
		}
		if p == '*' && pn == '/' {
			l.advance()
			l.advance()
			nesting--
			continue
		}
		l.advance()
	}
	if l.isAtEnd() {
		l.report(ErrUnterminatedComment)
		return
	}
	l.advance()
}

func (l *Lexer) addString() {
	for l.peek() != '"' && !l.isAtEnd() {
		if l.peek() == '\n' {
			l.Line++
		}
		l.advance()
	}

	if l.isAtEnd() {
		l.report(ErrUnterminatedStr)
		return
	}

	l.advance()
	val := string(l.src[l.start+1 : l.cur-1])
	l.addTokenWithLit(token.STRING, val)
}

func (l *Lexer) addNumber() {
	for isDigit(l.peek()) {
		l.advance()
	}

	if l.peek() == '.' && isDigit(l.peekNext()) {
		// consume '.'
		l.advance()

		for isDigit(l.peek()) {
			l.advance()
		}
	}

	n, err := strconv.ParseFloat(string(l.src[l.start:l.cur]), 64)
	if err != nil {
		panic(err)
	}
	l.addTokenWithLit(token.NUMBER, n)
}

func (l *Lexer) identifier() {
	for isAlphaNumeric(l.peek()) {
		l.advance()
	}
	txt := string(l.src[l.start:l.cur])
	if kind, ok := keywords[txt]; ok {
		l.addToken(kind)
	} else {
		l.addToken(token.IDENTIFIER)
	}
}

func (l *Lexer) addToken(kind token.TokenType) { l.addTokenWithLit(kind, nil) }

func (l *Lexer) addTokenWithLit(kind token.TokenType, lit any) {
	txt := string(l.src[l.start:l.cur])
	l.tokens = append(l.tokens, token.NewToken(kind, txt, lit, l.Line))
}

func (l *Lexer) addMatchToken(expected rune, t1, t2 token.TokenType) {
	if l.match(expected) {
		l.addToken(t1)
	} else {
		l.addToken(t2)
	}
}

func (l *Lexer) isAtEnd() bool { return l.cur >= len(l.src) }

func (l *Lexer) advance() rune {
	l.cur++
	return l.src[l.cur-1]
}

func (l *Lexer) peek() rune {
	if l.isAtEnd() {
		return 0
	}
	return l.src[l.cur]
}

func (l *Lexer) peekNext() rune {
	if l.cur+1 >= len(l.src) {
		return 0
	}
	return l.src[l.cur+1]
}

func (l *Lexer) match(expected rune) bool {
	if l.isAtEnd() {
		return false
	}
	if l.src[l.cur] != expected {
		return false
	}
	l.cur++
	return true
}

func isDigit(c rune) bool { return c >= '0' && c <= '9' }
func isAlpha(c rune) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}
func isAlphaNumeric(c rune) bool { return isAlpha(c) || isDigit(c) }

func (l *Lexer) report(msg error) {
	fullMsg := fmt.Sprintf("[line %d] [Lexer] Error: %s", l.Line, msg)
	if errors.Is(msg, ErrUnexpectedChar) {
		fmt.Fprintf(os.Stderr, "%s '%c'\n", fullMsg, l.src[l.cur-1])
	} else {
		fmt.Fprintln(os.Stderr, fullMsg)
	}
	l.curErr = msg
}

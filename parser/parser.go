package parser

import (
	"fmt"
	"os"
	"slices"

	"github.com/Subarctic2796/blam/ast"
	"github.com/Subarctic2796/blam/token"
)

// resolver stuff
type varStatus byte

const (
	vs_DECLARED varStatus = iota
	vs_DEFINED
	vs_IMPLICIT
)

// the class type
type classType byte

const (
	class_NONE classType = iota
	class_CLASS
	class_SUBCLASS
)

// NOTE: will add these checks later
// const (
// 'static' keyword doesn't exist
// 	errThisInStatic         = "Can't use 'this' in a static function"
// 	errInitIsStatic         = "Can't use 'init' as a static function"
// 	errStaticNotInClass     = "Can't use 'static' outside of a class"
// 	errStaticNeedsMethod    = "'static' must be before a class method"
// 	errSuperInStatic        = "Can't use 'super' in a static method"
// Not used yet
// 	errAlreadyInScope       = "Already a variable with this name in this scope"
// 	errLocalInitializesSelf = "Can't read local variable in its own initializer"
// )

// TODO: resolver stuff?

type Parser struct {
	tokens    []token.Token
	cur       int
	loopDepth int
	scopes    []map[string]varStatus
	curClass  classType
	curFN     ast.FnType
	curErr    error
}

func NewParser(tokens []token.Token) *Parser {
	return &Parser{
		tokens:    tokens,
		cur:       0,
		loopDepth: 0,
		scopes:    make([]map[string]varStatus, 0),
		curClass:  class_NONE,
		curFN:     ast.FN_NONE,
		curErr:    nil,
	}
}

// ============== HELPERS ==============

func (p *Parser) reportErr(tok *token.Token, msg string) {
	if tok.Kind == token.EOF {
		fmt.Fprintf(os.Stderr, "[line %d] [Parser] Error at end: %s\n", tok.Line, msg)
	} else {
		fmt.Fprintf(os.Stderr, "[line %d] [Parser] Error at '%s': %s\n", tok.Line, tok.Lexeme, msg)
	}
	p.curErr = fmt.Errorf("%s", msg)
}

func (p *Parser) parseErr(tok *token.Token, msg string) error {
	p.reportErr(tok, msg)
	return p.curErr
}

func (p *Parser) previous() *token.Token { return &p.tokens[p.cur-1] }
func (p *Parser) peek() *token.Token     { return &p.tokens[p.cur] }
func (p *Parser) isAtEnd() bool          { return p.peek().Kind == token.EOF }

func (p *Parser) advance() *token.Token {
	if !p.isAtEnd() {
		p.cur++
	}
	return p.previous()
}

func (p *Parser) check(kind token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.tokens[p.cur].Kind == kind
}

func (p *Parser) checkNext(kind token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	nKind := p.tokens[p.cur+1].Kind
	return nKind != token.EOF && nKind == kind
}

func (p *Parser) match(kinds ...token.TokenType) bool {
	if slices.ContainsFunc(kinds, p.check) {
		p.advance()
		return true
	}
	return false
}

func (p *Parser) consume(kind token.TokenType, msg string) (*token.Token, error) {
	if p.check(kind) {
		return p.advance(), nil
	}
	return nil, p.parseErr(p.peek(), msg)
}

// use this after an error occurs, so that we can report as many errors
// in 1 pass as possible
func (p *Parser) synchronise() {
	p.advance()
	for !p.isAtEnd() {
		if p.previous().Kind == token.SEMICOLON {
			return
		}
		switch p.peek().Kind {
		case token.CLASS, token.FUN, token.VAR, token.FOR,
			token.IF, token.WHILE, token.BREAK, token.PRINT,
			token.RETURN, token.CONTINUE:
			return
		}
		p.advance()
	}
}

// ============== SCOPING HELPERS ==============

func (p *Parser) beginScope() {
	p.scopes = append(p.scopes, make(map[string]varStatus))
}

func (p *Parser) endScope() { p.scopes = p.scopes[:len(p.scopes)-1] }

func (p *Parser) declare(name *token.Token) {
	if len(p.scopes) == 0 {
		return
	}
	scope := p.scopes[len(p.scopes)-1]
	if _, ok := scope[name.Lexeme]; ok {
		p.reportErr(name, "Already a variable with this name in this scope")
	}
	scope[name.Lexeme] = vs_DECLARED
}

func (p *Parser) define(name *token.Token) {
	if len(p.scopes) == 0 {
		return
	}
	p.scopes[len(p.scopes)-1][name.Lexeme] = vs_DEFINED
}

// ============== ACTUAL PARSING ==============

func (p *Parser) Parse() ([]ast.Stmt, error) {
	stmts := make([]ast.Stmt, 0, 8)
	for !p.isAtEnd() {
		// don't need to check error as the error has already been reported
		stmt, _ := p.declaration()
		stmts = append(stmts, stmt)
	}
	// NOTE: we return the broken ast, even if an error happened,
	// as we check for errors at the call-site and will bail then
	// it also could be useful for syntax highlighters
	return stmts, p.curErr
}

// we return an error anyway as it makes life easier
func (p *Parser) declaration() (ast.Stmt, error) {
	if p.match(token.CLASS) {
		return p.classDecl()
	}

	// check for lambdas
	if p.check(token.FUN) && p.checkNext(token.IDENTIFIER) {
		// it wasn't a lambda so parse the function
		_, err := p.consume(token.FUN, "")
		if err != nil {
			return nil, err
		}
		return p.function(ast.FN_FUNC)
	}

	if p.match(token.VAR) {
		val, err := p.varDecl()
		if err != nil {
			p.synchronise()
			return nil, err
		}
		return val, nil
	}

	val, err := p.statement()
	if err != nil {
		p.synchronise()
		return nil, err
	}
	return val, err
}

func (p *Parser) classDecl() (ast.Stmt, error) {
	// we do this so we can report an error
	prvCLS := p.curClass
	p.curClass = class_CLASS
	// make sure to reset the class state
	defer func() {
		if p.curClass == class_SUBCLASS {
			// close the super class scope if it exists
			p.endScope()
		}
		p.curClass = prvCLS
		// close this classes scope
		p.endScope()
	}()

	name, err := p.consume(token.IDENTIFIER, "Expect class name")
	if err != nil {
		return nil, err
	}

	p.declare(name)
	p.define(name)

	var supercls *ast.VariableExpr
	// TODO: we are currently using '<' for the 'extends' keyword
	// change it?
	if p.match(token.LT) {
		_, err := p.consume(token.IDENTIFIER, "Expect superclass name")
		if err != nil {
			return nil, err
		}

		supercls = &ast.VariableExpr{Name: p.previous()}
		if supercls.Name.Lexeme == name.Lexeme {
			// report error only don't bail
			p.reportErr(supercls.Name, "A class can't inherit from itself")
		}

		// we are now in a subclass
		p.beginScope()
		p.curClass = class_SUBCLASS
		p.scopes[len(p.scopes)-1]["super"] = vs_IMPLICIT
	}

	p.beginScope()
	p.scopes[len(p.scopes)-1]["this"] = vs_IMPLICIT

	_, err = p.consume(token.LBRACE, "Expect '{' before class body")
	if err != nil {
		return nil, err
	}

	// generate the methods
	methods := make([]*ast.FnStmt, 0)
	for !p.check(token.RBRACE) && !p.isAtEnd() {
		// TODO: add parsing of static methods
		method, err := p.function(ast.FN_METHOD)
		if err != nil {
			return nil, err
		}

		methods = append(methods, method)
	}

	_, err = p.consume(token.RBRACE, "Expect '}' after class body")
	if err != nil {
		return nil, err
	}

	return &ast.ClassStmt{Name: name, Superclass: supercls, Methods: methods}, nil
}

func (p *Parser) function(kind ast.FnType) (*ast.FnStmt, error) {
	msg := fmt.Sprintf("Expect %s name", kind)
	name, err := p.consume(token.IDENTIFIER, msg)
	if err != nil {
		return nil, err
	}

	p.declare(name)
	p.define(name)

	// check if its an constructor function
	if name.Lexeme == "init" {
		kind = ast.FN_INIT
	}

	// parse the body
	body, err := p.lambda(kind)
	if err != nil {
		return nil, err
	}

	// body is a lambda, which is defined as a wrapper around a FnStmt
	// so we unwrap the lambda into a function
	return &ast.FnStmt{
		Name:   name,
		Params: body.Params,
		Body:   body.Body,
		Kind:   body.Kind,
	}, nil
}

func (p *Parser) lambda(kind ast.FnType) (*ast.LambdaExpr, error) {
	p.beginScope()

	prvFn := p.curFN
	p.curFN = kind
	// reset curFN state
	defer func() {
		p.curFN = prvFn
		p.endScope()
	}()

	msg := fmt.Sprintf("Expect '(' after %s name", kind)
	keyword := p.previous()
	_, err := p.consume(token.LPAREN, msg)
	if err != nil {
		return nil, err
	}

	// parse the params
	params := make([]*token.Token, 0)
	// check if there even are parameters
	if !p.check(token.RPAREN) {
		// parse the params
		// go's version of a do while loop
		for ok := true; ok; ok = p.match(token.COMMA) {
			if len(params) >= 255 {
				// report error as our bytecode will only
				// let us have 255 parameters for a function
				p.reportErr(p.peek(), "Can't have more than 255 parameters")
			}

			param, err := p.consume(token.IDENTIFIER, "Expect parameter name")
			if err != nil {
				return nil, err
			}

			// scope stuff
			p.declare(param)
			p.define(param)

			params = append(params, param)
		}
	}

	_, err = p.consume(token.RPAREN, "Expect ')' after parameters")
	if err != nil {
		return nil, err
	}

	msg = fmt.Sprintf("Expect '{' before %s body", kind)
	_, err = p.consume(token.LBRACE, msg)
	if err != nil {
		return nil, err
	}

	// parse the body
	body, err := p.block()
	if err != nil {
		return nil, err
	}

	return &ast.LambdaExpr{
		FnStmt: &ast.FnStmt{
			Name:   keyword,
			Params: params,
			Body:   body,
			Kind:   kind,
		}}, nil
}

func (p *Parser) varDecl() (ast.Stmt, error) {
	name, err := p.consume(token.IDENTIFIER, "Expect variable name")
	if err != nil {
		return nil, err
	}

	p.declare(name)

	// parse the initializer expression
	var init ast.Expr
	if p.match(token.EQ) {
		init, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = p.consume(token.SEMICOLON, "Expect ';' after variable declaration")
	if err != nil {
		return nil, err
	}

	p.define(name)

	return &ast.VarStmt{Name: name, Initializer: init}, nil
}

func (p *Parser) statement() (ast.Stmt, error) {
	if p.match(token.BREAK, token.CONTINUE) {
		keyword := p.previous()
		msg := fmt.Sprintf("Must be in a loop to use '%s'", keyword.Lexeme)
		if p.loopDepth == 0 {
			p.reportErr(keyword, msg)
		}

		msg = fmt.Sprintf("Expect ';' after '%s'", keyword.Lexeme)
		_, err := p.consume(token.SEMICOLON, msg)
		if err != nil {
			return nil, err
		}

		return &ast.ControlStmt{Keyword: keyword, Value: nil}, nil
	} else if p.match(token.FOR) {
		p.loopDepth++ // so we can make sure 'break' and 'continue' are in a loop
		loop, err := p.forStmt()
		p.loopDepth--
		return loop, err
	} else if p.match(token.IF) {
		return p.ifStmt()
	} else if p.match(token.PRINT) { // TODO: temp while no native funcs
		return p.printStmt()
	} else if p.match(token.RETURN) {
		return p.returnStmt()
	} else if p.match(token.WHILE) {
		p.loopDepth++ // so we can make sure 'break' and 'continue' are in a loop
		loop, err := p.whileStmt()
		p.loopDepth--
		return loop, err
	} else if p.match(token.LBRACE) {
		p.beginScope()
		brace := p.previous()
		block, err := p.block()
		p.endScope()
		return &ast.BlockStmt{Brace: brace, Statements: block}, err
	}
	return p.expressionStmt()
}

func (p *Parser) forStmt() (ast.Stmt, error) {
	// for loops get desugared into a while loop
	//
	// for (var i = 0; i < 10; i = i + 1) { body } =>
	// {
	//     var i = 0;
	//     while (i < 10) {
	//         body
	//         i = i + 1;
	//     }
	// }

	p.beginScope()
	defer p.endScope()

	_, err := p.consume(token.LPAREN, "Expect '(' after 'for'")
	if err != nil {
		return nil, err
	}

	var init ast.Stmt
	if p.match(token.SEMICOLON) {
		init = nil
	} else if p.match(token.VAR) {
		init, err = p.varDecl()
		if err != nil {
			return nil, err
		}
	} else {
		init, err = p.expressionStmt()
		if err != nil {
			return nil, err
		}
	}

	var cond ast.Expr
	if !p.check(token.SEMICOLON) {
		cond, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = p.consume(token.SEMICOLON, "Expect ';' after loop condition")
	if err != nil {
		return nil, err
	}

	var incr ast.Expr
	if !p.check(token.RPAREN) {
		incr, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = p.consume(token.RPAREN, "Expect ')' after for clauses")
	if err != nil {
		return nil, err
	}

	body, err := p.statement()
	if err != nil {
		return nil, err
	}

	// add the increment statement
	if incr != nil {
		body = &ast.BlockStmt{
			Statements: []ast.Stmt{
				body,
				&ast.ExprStmt{Expression: incr},
			},
		}
	}

	// make sure there is a condition
	if cond == nil {
		cond = &ast.Literal{Value: true}
	}

	body = &ast.WhileStmt{Condition: cond, Body: body}

	// add the init statement
	if init != nil {
		body = &ast.BlockStmt{
			Statements: []ast.Stmt{init, body},
		}
	}

	return body, nil
}

func (p *Parser) ifStmt() (ast.Stmt, error) {
	_, err := p.consume(token.LPAREN, "Expect '(' after 'if'")
	if err != nil {
		return nil, err
	}

	// condition
	cond, err := p.expression()
	if err != nil {
		return nil, err
	}
	_, err = p.consume(token.RPAREN, "Expect ')' after if condition")
	if err != nil {
		return nil, err
	}

	thenBranch, err := p.statement()
	if err != nil {
		return nil, err
	}

	// else branch might not exist
	var elseBranch ast.Stmt = nil
	if p.match(token.ELSE) {
		elseBranch, err = p.statement()
		if err != nil {
			return nil, err
		}
	}

	return &ast.IfStmt{
		Cond:       cond,
		ThenBranch: thenBranch,
		ElseBranch: elseBranch,
	}, nil
}

func (p *Parser) printStmt() (ast.Stmt, error) {
	keyword := p.previous()
	val, err := p.expression()
	if err != nil {
		return nil, err
	}

	_, err = p.consume(token.SEMICOLON, "Expect ';' after value")
	if err != nil {
		return nil, err
	}

	return &ast.PrintStmt{Keyword: keyword, Expression: val}, nil
}

func (p *Parser) returnStmt() (ast.Stmt, error) {
	keyword := p.previous()
	if p.curFN == ast.FN_NONE {
		p.reportErr(keyword, "Can't return from top-level code")
	}

	var val ast.Expr
	var err error
	// see if its 'return;' or 'return val;'
	if !p.check(token.SEMICOLON) {
		val, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = p.consume(token.SEMICOLON, "Expect ';' after return value")
	if err != nil {
		return nil, err
	}

	// can't return a value from a constructor function
	if p.curFN == ast.FN_INIT && val != nil {
		p.reportErr(keyword, "Can't return a value from an initializer")
	}

	return &ast.ControlStmt{Keyword: keyword, Value: val}, nil
}

func (p *Parser) whileStmt() (ast.Stmt, error) {
	keyword := p.previous()
	_, err := p.consume(token.LPAREN, "Expect '(' after 'while'")
	if err != nil {
		return nil, err
	}

	// condition
	cond, err := p.expression()
	if err != nil {
		return nil, err
	}

	_, err = p.consume(token.RPAREN, "Expect ')' after condition")
	if err != nil {
		return nil, err
	}

	body, err := p.statement()
	if err != nil {
		return nil, err
	}

	return &ast.WhileStmt{Keyword: keyword, Condition: cond, Body: body}, nil
}

func (p *Parser) expressionStmt() (ast.Stmt, error) {
	tok := p.previous()
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}

	_, err = p.consume(token.SEMICOLON, "Expect ';' after expression")
	if err != nil {
		return nil, err
	}

	return &ast.ExprStmt{Token: tok, Expression: expr}, nil
}

func (p *Parser) block() ([]ast.Stmt, error) {
	stmts := make([]ast.Stmt, 0)
	for !p.check(token.RBRACE) && !p.isAtEnd() {
		stmt, err := p.declaration()
		if err != nil {
			return nil, err
		}

		stmts = append(stmts, stmt)
	}

	_, err := p.consume(token.RBRACE, "Expect '}' after block")
	if err != nil {
		return nil, err
	}

	return stmts, nil
}

// recursive decent
func (p *Parser) expression() (ast.Expr, error) { return p.assignment() }

// a = 2 + 3;
func (p *Parser) assignment() (ast.Expr, error) {
	expr, err := p.or()
	if err != nil {
		return nil, err
	}

	if p.match(token.EQ) {
		opr := p.previous()
		val, err := p.assignment()
		if err != nil {
			return nil, err
		}

		switch e := expr.(type) {
		case *ast.VariableExpr:
			return &ast.AssignExpr{Name: e.Name, Value: val}, nil
		case *ast.GetExpr:
			return &ast.SetExpr{
				Object: e.Object,
				Name:   e.Name,
				Value:  val,
			}, nil
		case *ast.IndexedGetExpr:
			if e.Stop != nil {
				p.reportErr(e.Sqr, "Can't use slicing to set values")
			}

			return &ast.IndexedSetExpr{
				Object: e.Object,
				Sqr:    e.Sqr,
				Index:  e.Start,
				Value:  val,
			}, nil
		}

		p.reportErr(opr, "Invalid assignment target")
	}

	return expr, nil
}

// true or false
func (p *Parser) or() (ast.Expr, error) {
	expr, err := p.and()
	if err != nil {
		return nil, err
	}
	for p.match(token.OR) {
		opr := p.previous()
		rhs, err := p.and()
		if err != nil {
			return nil, err
		}
		expr = &ast.LogicalExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// true and false
func (p *Parser) and() (ast.Expr, error) {
	expr, err := p.equality()
	if err != nil {
		return nil, err
	}
	for p.match(token.AND) {
		opr := p.previous()
		rhs, err := p.equality()
		if err != nil {
			return nil, err
		}
		expr = &ast.LogicalExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// a == b / a != b
func (p *Parser) equality() (ast.Expr, error) {
	expr, err := p.comparison()
	if err != nil {
		return nil, err
	}
	for p.match(token.NEQ, token.EQ_EQ) {
		opr := p.previous()
		rhs, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// 23 < 5
func (p *Parser) comparison() (ast.Expr, error) {
	expr, err := p.addition()
	if err != nil {
		return nil, err
	}
	for p.match(token.GT, token.GT_EQ, token.LT, token.LT_EQ) {
		opr := p.previous()
		rhs, err := p.addition()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// 23 + 5
func (p *Parser) addition() (ast.Expr, error) {
	expr, err := p.multiplication()
	if err != nil {
		return nil, err
	}
	for p.match(token.MINUS, token.PLUS) {
		opr := p.previous()
		rhs, err := p.multiplication()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// 23 * 5
func (p *Parser) multiplication() (ast.Expr, error) {
	expr, err := p.unary()
	if err != nil {
		return nil, err
	}
	for p.match(token.SLASH, token.STAR) {
		opr := p.previous()
		rhs, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = &ast.BinaryExpr{Left: expr, Operator: opr, Right: rhs}
	}
	return expr, nil
}

// !true / -a
func (p *Parser) unary() (ast.Expr, error) {
	if p.match(token.BANG, token.MINUS) {
		opr := p.previous()
		rhs, err := p.unary()
		if err != nil {
			return nil, err
		}
		return &ast.UnaryExpr{Operator: opr, Right: rhs}, nil
	}
	return p.call()
}

// fnName(params, ...)
func (p *Parser) call() (ast.Expr, error) {
	expr, err := p.primary()
	if err != nil {
		return nil, err
	}

	for {
		if p.match(token.LPAREN) {
			expr, err = p.finishCall(expr)
			if err != nil {
				return nil, err
			}
		} else if p.match(token.LSQR) {
			expr, err = p.finishIndex(expr)
			if err != nil {
				return nil, err
			}
		} else if p.match(token.DOT) {
			name, err := p.consume(token.IDENTIFIER, "Expect property name after '.'")
			if err != nil {
				return nil, err
			}

			expr = &ast.GetExpr{Object: expr, Name: name}
		} else {
			break
		}
	}

	return expr, nil
}

func (p *Parser) finishCall(callee ast.Expr) (ast.Expr, error) {
	args := make([]ast.Expr, 0)
	if !p.check(token.RPAREN) {
		for ok := true; ok; ok = p.match(token.COMMA) {
			if len(args) >= 255 {
				// due to bytecode
				p.reportErr(p.peek(), "Can't have more than 255 arguments")
			}

			arg, err := p.expression()
			if err != nil {
				return nil, err
			}

			args = append(args, arg)
		}
	}

	paren, err := p.consume(token.RPAREN, "Expect ')' after arguments")
	if err != nil {
		return nil, err
	}

	return &ast.CallExpr{Callee: callee, Paren: paren, Arguments: args}, nil
}

func (p *Parser) finishIndex(iter ast.Expr) (ast.Expr, error) {
	if p.match(token.RSQR) {
		return nil, p.parseErr(p.previous(), "Expect an expression or ':' in an index expression")
	}

	var sqr *token.Token = nil
	var colon *token.Token = nil
	var err error
	var startIdx ast.Expr = nil

	if !p.check(token.COLON) {
		// arr[s:?]
		startIdx, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	// arr[s?:?]
	var stopIdx ast.Expr = nil
	if p.match(token.COLON) {
		colon = p.previous()
		if !p.check(token.RSQR) {
			stopIdx, err = p.expression()
			if err != nil {
				return nil, err
			}
		}

		sqr, err = p.consume(token.RSQR, "Expect ']' after index")
		if err != nil {
			return nil, err
		}
	}

	if sqr == nil {
		sqr, err = p.consume(token.RSQR, "Expect ']' after index")
		if err != nil {
			return nil, err
		}
	}

	return &ast.IndexedGetExpr{
		Object: iter,
		Sqr:    sqr,
		Start:  startIdx,
		Colon:  colon,
		Stop:   stopIdx,
	}, nil
}

func (p *Parser) primary() (ast.Expr, error) {
	if p.match(token.FALSE) {
		return &ast.Literal{Value: false}, nil
	} else if p.match(token.TRUE) {
		return &ast.Literal{Value: true}, nil
	} else if p.match(token.NIL) {
		return &ast.Literal{Value: nil}, nil
	} else if p.match(token.NUMBER, token.STRING) {
		return &ast.Literal{Value: p.previous().Literal}, nil
	} else if p.match(token.SUPER) {
		keyword := p.previous()

		if p.curClass == class_NONE {
			p.reportErr(keyword, "Can't use 'super' outside of a class")
		} else if p.curClass != class_SUBCLASS {
			p.reportErr(keyword, "Can't use 'super' in a class with no superclass")
		}

		_, err := p.consume(token.DOT, "Expect '.' after 'super'")
		if err != nil {
			return nil, err
		}

		method, err := p.consume(token.IDENTIFIER, "Expect superclass method name")
		if err != nil {
			return nil, err
		}

		return &ast.SuperExpr{Keyword: keyword, Method: method}, nil
	} else if p.match(token.THIS) {
		keyword := p.previous()
		if p.curClass == class_NONE {
			p.reportErr(keyword, "Can't use 'this' outside of a class")
		}

		return &ast.ThisExpr{Keyword: keyword}, nil
	} else if p.match(token.FUN) {
		return p.lambda(ast.FN_LAMBDA)
	} else if p.match(token.IF) {
		// parse if expression
		// NOTE: not sure if this is being parsed correctly
		ifS, err := p.ifStmt()
		if err != nil {
			return nil, err
		}

		return &ast.IfExpr{If: ifS.(*ast.IfStmt)}, nil
	} else if p.match(token.IDENTIFIER) {
		name := p.previous()
		if len(p.scopes) != 0 {
			state, ok := p.scopes[len(p.scopes)-1][name.Lexeme]
			if ok && state == vs_DECLARED {
				p.reportErr(name, "Can't read local variable in its own initializer")
			}
		}
		return &ast.VariableExpr{Name: name}, nil
	} else if p.match(token.LPAREN) {
		expr, err := p.expression()
		if err != nil {
			return nil, err
		}

		_, err = p.consume(token.RPAREN, "Expect ')' after expression")
		if err != nil {
			return nil, err
		}

		return &ast.GroupingExpr{Expression: expr}, nil
	} else if p.match(token.LSQR) {
		sqr := p.previous()

		elements, err := p.finishArray()
		if err != nil {
			return nil, err
		}

		return &ast.ArrayLiteral{Sqr: sqr, Elements: elements}, nil
	} else if p.match(token.LBRACE) {
		brace := p.previous()

		pairs, err := p.finishHashMap()
		if err != nil {
			return nil, err
		}

		return &ast.HashLiteral{Brace: brace, Pairs: pairs}, nil
	}

	return nil, p.parseErr(p.peek(), "Expect expression")
}

func (p *Parser) finishArray() ([]ast.Expr, error) {
	elements := make([]ast.Expr, 0)
	if !p.check(token.RSQR) {
		for ok := true; ok; ok = p.match(token.COMMA) {
			// found trailing comma
			if p.check(token.RSQR) {
				break
			}

			elm, err := p.expression()
			if err != nil {
				return nil, err
			}

			elements = append(elements, elm)
		}
	}

	_, err := p.consume(token.RSQR, "Expect ']' after array elements")
	if err != nil {
		return nil, err
	}

	return elements, nil
}

func (p *Parser) finishHashMap() (map[ast.Expr]ast.Expr, error) {
	pairs := make(map[ast.Expr]ast.Expr)
	if !p.check(token.RBRACE) {
		for ok := true; ok; ok = p.match(token.COMMA) {
			// found trailing comma
			if p.check(token.RBRACE) {
				break
			}

			// we don't want 'a = 3: value', so we increase the precedence
			key, err := p.or()
			if err != nil {
				return nil, err
			}

			// check for unhashable types
			switch kt := key.(type) {
			case *ast.ArrayLiteral:
				p.reportErr(kt.Sqr, "Unhashable type")
			case *ast.HashLiteral:
				p.reportErr(kt.Brace, "Unhashable type")
			case *ast.LambdaExpr:
				p.reportErr(kt.Name, "Unhashable type")
			}

			_, err = p.consume(token.COLON, "Expect ':' after hashmap key")
			if err != nil {
				return nil, err
			}

			// parse the value
			val, err := p.or()
			if err != nil {
				return nil, err
			}

			pairs[key] = val
		}
	}

	_, err := p.consume(token.RBRACE, "Expect '}' after array elements")
	if err != nil {
		return nil, err
	}

	return pairs, nil
}

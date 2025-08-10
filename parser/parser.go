package parser

import (
	"fmt"
	"os"
	"slices"

	"github.com/Subarctic2796/blam/ast"
	"github.com/Subarctic2796/blam/token"
)

// the class type
type classType byte

const (
	class_NONE classType = iota
	class_CLASS
	class_SUBCLASS
)

const (
	errReturnTopLevel = "Can't return from top-level code"
	errReturnFromInit = "Can't return a value from an initializer"

	errInheritsSelf       = "A class can't inherit from itself"
	errThisNotInClass     = "Can't use 'this' outside of a class"
	errSuperNotInClass    = "Can't use 'super' outside of a class"
	errSuperNotInSubClass = "Can't use 'super' in a class with no superclass"

	errUnHashable = "Can only use: strings, numbers, bools, and instances as hashmap keys"

	// NOTE: 'static' keyword doesn't exist yet
	// errThisInStatic       = "Can't use 'this' in a static function"
	// errInitIsStatic       = "Can't use 'init' as a static function"
	// errStaticNotInClass   = "Can't use 'static' outside of a class"
	// errStaticNeedsMethod  = "'static' must be before a class method"
	// errSuperInStatic      = "Can't use 'super' in a static method"

	// NOTE: not used yet
	// errAlreadyInScope       = "Already a variable with this name in this scope"
	// errLocalInitializesSelf = "Can't read local variable in its own initializer"
)

type Parser struct {
	tokens    []token.Token
	cur       int
	loopDepth int
	curClass  classType
	curFN     ast.FnType
	curErr    error
}

func NewParser(tokens []token.Token) *Parser {
	return &Parser{
		tokens:    tokens,
		cur:       0,
		loopDepth: 0,
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

	// chech for lambdas
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
	defer func() { p.curClass = prvCLS }()

	name, err := p.consume(token.IDENTIFIER, "Expect class name")
	if err != nil {
		return nil, err
	}

	var supercls *ast.VariableExpr
	// TODO: we are currently using '<' for as the 'extends' keyword
	// change it?
	if p.match(token.LT) {
		_, err := p.consume(token.IDENTIFIER, "Expect superclass name")
		if err != nil {
			return nil, err
		}
		supercls = &ast.VariableExpr{Name: p.previous()}
		if supercls.Name.Lexeme == name.Lexeme {
			// report error only don't bail
			// error is: a class can't inherit from itself
			p.reportErr(supercls.Name, errInheritsSelf)
		}
		// we are now in a subclass
		p.curClass = class_SUBCLASS
	}

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
		Params: body.Func.Params,
		Body:   body.Func.Body,
		Kind:   body.Func.Kind,
	}, nil
}

func (p *Parser) lambda(kind ast.FnType) (*ast.LambdaExpr, error) {
	prvFn := p.curFN
	p.curFN = kind
	// reset curFN state
	defer func() { p.curFN = prvFn }()

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
			ident, err := p.consume(token.IDENTIFIER, "Expect parameter name")
			if err != nil {
				return nil, err
			}
			params = append(params, ident)
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
	fn := &ast.FnStmt{Name: keyword, Params: params, Body: body, Kind: kind}
	return &ast.LambdaExpr{Func: fn}, nil
}

func (p *Parser) varDecl() (ast.Stmt, error) {
	name, err := p.consume(token.IDENTIFIER, "Expect variable name")
	if err != nil {
		return nil, err
	}

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
	return &ast.VarStmt{Name: name, Initializer: init}, nil
}

func (p *Parser) statement() (ast.Stmt, error) {
	if p.match(token.BREAK, token.CONTINUE) {
		return p.loopControlStmt()
	} else if p.match(token.FOR) {
		return p.forStmt()
	} else if p.match(token.IF) {
		return p.ifStmt()
	} else if p.match(token.PRINT) { // TODO: temp while no native funcs
		return p.printStmt()
	} else if p.match(token.RETURN) {
		return p.returnStmt()
	} else if p.match(token.WHILE) {
		return p.whileStmt()
	} else if p.match(token.LBRACE) {
		block, err := p.block()
		if err != nil {
			return nil, err
		}
		return &ast.BlockStmt{Statements: block}, nil
	}
	return p.expressionStmt()
}

func (p *Parser) loopControlStmt() (ast.Stmt, error) {
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

	p.loopDepth++ // so we can make sure 'break' and 'continue' are in a loop
	defer func() { p.loopDepth-- }()

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
	val, err := p.expression()
	if err != nil {
		return nil, err
	}
	_, err = p.consume(token.SEMICOLON, "Expect ';' after value")
	if err != nil {
		return nil, err
	}
	return &ast.PrintStmt{Expression: val}, nil
}

func (p *Parser) returnStmt() (ast.Stmt, error) {
	keyword := p.previous()
	if p.curFN == ast.FN_NONE {
		p.reportErr(keyword, errReturnTopLevel)
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
		p.reportErr(keyword, errReturnFromInit)
	}
	return &ast.ControlStmt{Keyword: keyword, Value: val}, nil
}

func (p *Parser) whileStmt() (ast.Stmt, error) {
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

	// update loopDepth
	p.loopDepth++
	defer func() { p.loopDepth-- }()

	body, err := p.statement()
	if err != nil {
		return nil, err
	}

	return &ast.WhileStmt{Condition: cond, Body: body}, nil
}

func (p *Parser) expressionStmt() (ast.Stmt, error) {
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}
	_, err = p.consume(token.SEMICOLON, "Expect ';' after expression")
	if err != nil {
		return nil, err
	}
	return &ast.ExprStmt{Expression: expr}, nil
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
			p.reportErr(keyword, errSuperNotInClass)
		} else if p.curClass != class_SUBCLASS {
			p.reportErr(keyword, errSuperNotInSubClass)
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
			p.reportErr(keyword, errThisNotInClass)
		}

		return &ast.ThisExpr{Keyword: keyword}, nil
	} else if p.match(token.FUN) {
		return p.lambda(ast.FN_LAMBDA)
	} else if p.match(token.IDENTIFIER) {
		return &ast.VariableExpr{Name: p.previous()}, nil
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

			// we don't want a = 3: value, so we increase the precedence
			key, err := p.or()
			if err != nil {
				return nil, err
			}

			// check for unhashable types
			switch kt := key.(type) {
			case *ast.ArrayLiteral:
				p.reportErr(kt.Sqr, errUnHashable)
			case *ast.HashLiteral:
				p.reportErr(kt.Brace, errUnHashable)
			case *ast.LambdaExpr:
				p.reportErr(kt.Func.Name, errUnHashable)
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

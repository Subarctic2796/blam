#include "parser.h"
#include "arena.h"
#include "ast.h"
#include "common.h"
#include "lexer.h"
#include "token.h"

typedef enum {
    CLS_NONE,
    CLS_CLASS,
    CLS_SUBCLASS,
} ClassType;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * / %
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_SUBSCRIPT,  // [expr]
    PREC_PRIMARY
} Precedence;

typedef Expr *(*PrefixFn)(Parser *p, bool canAssign);
typedef Expr *(*InfixFn)(Parser *p, bool canAssign, Expr *lhs);

typedef struct {
    PrefixFn prefix;
    InfixFn infix;
    Precedence prec;
} ParseRule;

typedef struct Parser {
    bool hadErr, panicMode;
    FnType curFN;
    ClassType curCLS;
    int scopeDepth, loopDepth;
    Arena arena;
    Lexer lexer;
    Token prv, cur;
} Parser;

static_assert(PARSER_SIZE == sizeof(Parser), "size of parser changed");

// -------
// HELPERS
// -------

static inline Parser saveParser(const Parser *p, Arena_Mark *out) {
    Parser saved = *p;
    *out = arena_snapshot(&((Parser *)p)->arena);
    return saved;
}

static inline void rewindParser(Parser *p, Arena_Mark mark,
                                Parser *savedParser) {
    arena_rewind(&p->arena, mark);
    Arena arena = p->arena;
    *p = *savedParser;
    p->arena = arena;
}

static void errorAt(Parser *p, const Token token, const char *msg) {
    if (p->panicMode) return;

    p->panicMode = true;
    fprintf(stderr, "[line %d] Error", token.line);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (token.type) {
    case TOKEN_EOF:   fprintf(stderr, " at end"); break;
    case TOKEN_ERROR: break;
    default:          fprintf(stderr, " at '%.*s'", (int)token.cnt, token.items); break;
    }
#pragma GCC diagnostic pop

    fprintf(stderr, ": %s\n", msg);
    p->hadErr = true;
}

static inline void error(Parser *p, const char *msg) {
    errorAt(p, p->prv, msg);
}

static inline void errorAtCur(Parser *p, const char *msg) {
    errorAt(p, p->cur, msg);
}

static void advance(Parser *p) {
    p->prv = p->cur;

    for (;;) {
        p->cur = scanToken(&p->lexer);
        if (p->cur.type != TOKEN_ERROR) break;

        errorAtCur(p, p->cur.items);
    }
}

static inline void consume(Parser *p, TokenType t, const char *msg) {
    if (p->cur.type == t) {
        advance(p);
        return;
    }
    errorAtCur(p, msg);
}

static inline void consumeSemiColon(Parser *p, const char *msg) {
    static char msgBuffer[1024] = {0};
    snprintf(msgBuffer, sizeof(msgBuffer), "Expect ';' after %s", msg);
    consume(p, TOKEN_SEMICOLON, msgBuffer);
}

static inline bool check(Parser *p, TokenType t) { return p->cur.type == t; }

static inline bool match(Parser *p, TokenType t) {
    if (!check(p, t)) return false;
    advance(p);
    return true;
}

static bool matchAny(Parser *p, size_t n, const TokenType *types) {
    for (size_t i = 0; i < n; i++) {
        if (match(p, types[i])) return true;
    }
    return false;
}

static inline void synchronize(Parser *parser) {
    parser->panicMode = false;

    while (parser->cur.type != TOKEN_EOF) {
        if (parser->prv.type == TOKEN_SEMICOLON) return;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        switch (parser->cur.type) {
        case TOKEN_BREAK:
        case TOKEN_CONTINUE:
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:   return;

        default:; // Do nothing.
        }
#pragma GCC diagnostic pop

        advance(parser);
    }
}

static const TokenType EQS[] = {
    TOKEN_EQ,       TOKEN_PLUS_EQ, TOKEN_MINUS_EQ,
    TOKEN_SLASH_EQ, TOKEN_STAR_EQ, TOKEN_PERCENT_EQ,
};

#define EQS_LEN ARRAY_LEN(EQS)

// -----------
// PARSING FNs
// -----------

static inline Expr *expression(Parser *p);
static inline ParseRule *getRule(TokenType t);
static Stmt *statement(Parser *p);
static Stmt *declaration(Parser *p);
static Stmts block(Parser *p);
static Expr *parsePrecedence(Parser *p, Precedence prec);

// -----------
// PREFIX FNs
// -----------

static Expr *grouping(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    Token tok = p->prv;
    Expr *group = expression(p);
    consume(p, TOKEN_RPAREN, "Expect ')' after expression");
    return newExpr(EXPR_GROUPING, tok, .group = group);
}

static Expr *map(Parser *p, bool canAssign) {
    UNUSED(canAssign);

    Token brace = p->prv;

    if (match(p, TOKEN_RBRACE)) {
        return newExpr(EXPR_HASH, brace, .elements = ((Exprs){0}));
    }

    Exprs elems = {0};
    do {
        // trailing comma
        if (check(p, TOKEN_RBRACE)) break;

        Expr *key = parsePrecedence(p, PREC_OR);
        arena_da_append(&p->arena, &elems, key);

        consume(p, TOKEN_COLON, "Expect ':' after map key");

        Expr *val = parsePrecedence(p, PREC_OR);
        arena_da_append(&p->arena, &elems, val);
    } while (match(p, TOKEN_COMMA));

    consume(p, TOKEN_RPAREN, "Expect '}' after map literal");

    return newExpr(EXPR_HASH, brace, .elements = elems);
}

static Expr *array(Parser *p, bool canAssign) {
    UNUSED(canAssign);

    Token brace = p->prv;

    if (match(p, TOKEN_RSQR)) {
        return newExpr(EXPR_ARRAY, brace, .elements = ((Exprs){0}));
    }

    Exprs elems = {0};
    do {
        // trailing comma
        if (check(p, TOKEN_RSQR)) break;

        Expr *val = parsePrecedence(p, PREC_OR);
        arena_da_append(&p->arena, &elems, val);
    } while (match(p, TOKEN_COMMA));

    consume(p, TOKEN_RSQR, "Expect ']' after array literal");

    return newExpr(EXPR_ARRAY, brace, .elements = elems);
}

static Expr *unary(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    Token tok = p->prv;
    Expr *rhs = parsePrecedence(p, PREC_UNARY);
    return newExpr(EXPR_UNARY, tok, .rhs = rhs);
}

static Expr *variable(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    return newExpr(EXPR_IDENT, p->prv, .scope = -1, .idx = -1, .name = p->prv);
}

static Expr *literal(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    return newExpr(EXPR_LITERAL, p->prv);
}

static Stmt *function(Parser *p, FnType type);

static Expr *lambda(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    Token name = p->prv;
    Stmt *fn = function(p, FN_LAMBDA);
    return newExpr(EXPR_LAMBDA, name, .lambda = fn);
}

static Stmt *ifStmt(Parser *p);

static Expr *ifExpr(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    Token tok = p->prv;
    Stmt *if_ = ifStmt(p);
    return newExpr(EXPR_IF, tok, .if_ = if_);
}

static Expr *super_(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    Token kw = p->prv;
    if (p->curCLS == CLS_NONE) {
        error(p, "Can't use 'super' outside of a class");
    } else if (p->curCLS != CLS_SUBCLASS) {
        error(p, "Can't use 'super' in a class with no superclass");
    }

    consume(p, TOKEN_DOT, "Expect '.' after 'super'");
    consume(p, TOKEN_IDENTIFIER, "Expect superclass method name");

    return newExpr(EXPR_SUPER, kw, .method = p->prv);
}

static Expr *this_(Parser *p, bool canAssign) {
    UNUSED(canAssign);
    if (p->curCLS == CLS_NONE) error(p, "Can't use 'this' outside of a class");
    return newExpr(EXPR_THIS, p->prv);
}

// -----------
// INFIX FNs
// -----------

static Expr *call(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token tok = p->prv;

    if (match(p, TOKEN_RPAREN)) {
        return newExpr(EXPR_CALL, tok, .callee = lhs, .args = ((Exprs){0}));
    }

    Exprs args = {0};
    do {
        Expr *arg = expression(p);
        if (args.cnt >= 255) error(p, "Can't have more than 255 arguments");
        arena_da_append(&p->arena, &args, arg);
    } while (match(p, TOKEN_COMMA));
    consume(p, TOKEN_RPAREN, "Expect ')' after arguments");
    return newExpr(EXPR_CALL, tok, .callee = lhs, .args = args);
}

static Expr *desugarOp(Parser *p, Expr *lhs, Token opr, Expr *value) {
    TokenType oprType = __TOKEN_CNT;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (opr.type) {
    case TOKEN_EQ:         return value;
    case TOKEN_PLUS_EQ:    oprType = TOKEN_PLUS; break;
    case TOKEN_MINUS_EQ:   oprType = TOKEN_MINUS; break;
    case TOKEN_SLASH_EQ:   oprType = TOKEN_SLASH_EQ; break;
    case TOKEN_STAR_EQ:    oprType = TOKEN_STAR; break;
    case TOKEN_PERCENT_EQ: oprType = TOKEN_PERCENT; break;
    default:               UNREACHABLE("operator is not = or [*]="); break;
    }
#pragma GCC diagnostic pop

    Token tok = {oprType, opr.line, .lexeme = opr.lexeme};
    return newExpr(EXPR_BINARY, tok, .lhs = lhs, .rhs = value);
}

static Expr *subscript(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token tok = p->prv;
    Expr *index = parsePrecedence(p, PREC_OR);
    consume(p, TOKEN_RSQR, "Expect ']' after index");

    if (canAssign && matchAny(p, EQS_LEN, EQS)) {
        Token opr = p->prv;
        Expr *value = expression(p);
        value = desugarOp(p, lhs, opr, value);
        return newExpr(EXPR_INDEXED_SET, tok, .object = lhs, .index = index,
                       .value = value);
    }
    return newExpr(EXPR_INDEXED_GET, tok, .object = lhs, .index = index);
}

static Expr *dot(Parser *p, bool canAssign, Expr *lhs) {
    consume(p, TOKEN_IDENTIFIER, "Expect property name after '.'");
    Token name = p->prv;

    if (canAssign && matchAny(p, EQS_LEN, EQS)) {
        Token opr = p->prv;
        Expr *value = expression(p);
        value = desugarOp(p, lhs, opr, value);
        return newExpr(EXPR_SET, name, .object = lhs, .value = value);
    }
    return newExpr(EXPR_GET, name, .get = lhs);
}

static Expr *assignment(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token opr = p->prv;
    Expr *val = parsePrecedence(p, PREC_ASSIGNMENT);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (lhs->type) {
    case EXPR_IDENT: {
        ExprIdent ident = lhs->as.ident;
        return newExpr(EXPR_ASSIGN, ident.name, .opr = opr,
                       .scope = ident.scope, .idx = ident.index, .value = val);
    }
    case EXPR_GET: {
        val = desugarOp(p, lhs->as.get, opr, val);
        return newExpr(EXPR_SET, lhs->token, .object = lhs->as.get,
                       .value = val);
    }
    case EXPR_INDEXED_GET: {
        ExprIndexedGet get = lhs->as.indexedGet;
        val = desugarOp(p, lhs, opr, val);
        return newExpr(EXPR_INDEXED_SET, lhs->token, .object = get.object,
                       .index = get.index, .value = val);
    } break;
    default: UNREACHABLE("left expr was not ident, get or indexed get"); break;
    }
#pragma GCC diagnostic pop
}

static Expr *binary(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token opr = p->prv;
    Precedence prec = getRule(opr.type)->prec;
    Expr *rhs = parsePrecedence(p, prec + 1);
    return newExpr(EXPR_BINARY, opr, .lhs = lhs, .rhs = rhs);
}

static Expr *and_(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token opr = p->prv;
    Precedence prec = getRule(opr.type)->prec;
    Expr *rhs = parsePrecedence(p, prec + 1);
    return newExpr(EXPR_LOGICAL, opr, .lhs = lhs, .rhs = rhs);
}

static Expr *or_(Parser *p, bool canAssign, Expr *lhs) {
    UNUSED(canAssign);
    Token opr = p->prv;
    Precedence prec = getRule(opr.type)->prec;
    Expr *rhs = parsePrecedence(p, prec + 1);
    return newExpr(EXPR_LOGICAL, opr, .lhs = lhs, .rhs = rhs);
}

static ParseRule RULES[] = {
    {grouping, call, PREC_CALL},         // TOKEN_LPAREN
    {NULL, NULL, PREC_NONE},             // TOKEN_RPAREN
    {map, NULL, PREC_NONE},              // TOKEN_LBRACE
    {NULL, NULL, PREC_NONE},             // TOKEN_RBRACE
    {array, subscript, PREC_SUBSCRIPT},  // TOKEN_LSQR
    {NULL, NULL, PREC_NONE},             // TOKEN_RSQR
    {NULL, NULL, PREC_NONE},             // TOKEN_COMMA
    {NULL, dot, PREC_CALL},              // TOKEN_DOT
    {NULL, NULL, PREC_NONE},             // TOKEN_SEMICOLON
    {NULL, NULL, PREC_NONE},             // TOKEN_COLON
    {unary, binary, PREC_TERM},          // TOKEN_MINUS
    {NULL, binary, PREC_TERM},           // TOKEN_PLUS
    {NULL, binary, PREC_FACTOR},         // TOKEN_SLASH
    {NULL, binary, PREC_FACTOR},         // TOKEN_STAR
    {NULL, binary, PREC_FACTOR},         // TOKEN_PERCENT
    {unary, NULL, PREC_NONE},            // TOKEN_BANG
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_EQ
    {NULL, binary, PREC_EQUALITY},       // TOKEN_NEQ
    {NULL, binary, PREC_EQUALITY},       // TOKEN_EQEQ
    {NULL, binary, PREC_COMPARISON},     // TOKEN_GT
    {NULL, binary, PREC_COMPARISON},     // TOKEN_GTEQ
    {NULL, binary, PREC_COMPARISON},     // TOKEN_LT
    {NULL, binary, PREC_COMPARISON},     // TOKEN_LTEQ
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_PLUS_EQ
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_MINUS_EQ
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_SLASH_EQ
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_STAR_EQ
    {NULL, assignment, PREC_ASSIGNMENT}, // TOKEN_PERCENT_EQ
    {variable, NULL, PREC_NONE},         // TOKEN_IDENTIFIER
    {literal, NULL, PREC_NONE},          // TOKEN_STRING
    {literal, NULL, PREC_NONE},          // TOKEN_NUMBER
    {NULL, and_, PREC_AND},              // TOKEN_AND
    {NULL, NULL, PREC_NONE},             // TOKEN_CLASS
    {NULL, NULL, PREC_NONE},             // TOKEN_ELSE
    {literal, NULL, PREC_NONE},          // TOKEN_FALSE
    {NULL, NULL, PREC_NONE},             // TOKEN_FOR
    {lambda, NULL, PREC_NONE},           // TOKEN_FUN
    {ifExpr, NULL, PREC_NONE},           // TOKEN_IF
    {literal, NULL, PREC_NONE},          // TOKEN_NIL
    {NULL, or_, PREC_OR},                // TOKEN_OR
    {NULL, NULL, PREC_NONE},             // TOKEN_PRINT
    {NULL, NULL, PREC_NONE},             // TOKEN_RETURN
    {super_, NULL, PREC_NONE},           // TOKEN_SUPER
    {this_, NULL, PREC_NONE},            // TOKEN_THIS
    {literal, NULL, PREC_NONE},          // TOKEN_TRUE
    {NULL, NULL, PREC_NONE},             // TOKEN_VAR
    {NULL, NULL, PREC_NONE},             // TOKEN_WHILE
    {NULL, NULL, PREC_NONE},             // TOKEN_BREAK
    {NULL, NULL, PREC_NONE},             // TOKEN_CONTINUE
    {NULL, NULL, PREC_NONE},             // TOKEN_IN
    {NULL, NULL, PREC_NONE},             // TOKEN_ERROR
    {NULL, NULL, PREC_NONE},             // TOKEN_EOF
};

static_assert(ARRAY_LEN(RULES) == __TOKEN_CNT,
              "number of rules != number of tokens");

static inline ParseRule *getRule(TokenType t) { return &RULES[t]; }

static Expr *parsePrecedence(Parser *p, Precedence prec) {
    advance(p);
    PrefixFn prefixRule = getRule(p->prv.type)->prefix;
    if (prefixRule == NULL) {
        error(p, "Expect expression");
        return NULL;
    }

    bool canAssign = prec <= PREC_ASSIGNMENT;
    Expr *lhs = prefixRule(p, canAssign);

    while (prec <= getRule(p->cur.type)->prec) {
        advance(p);
        InfixFn infixRule = getRule(p->prv.type)->infix;
        lhs = infixRule(p, canAssign, lhs);

        if (canAssign && matchAny(p, EQS_LEN, EQS)) {
            error(p, "Invalid assignment target");
        }
    }

    return lhs;
}

static inline Expr *expression(Parser *p) {
    return parsePrecedence(p, PREC_ASSIGNMENT);
}

static Stmts block(Parser *p) {
    Stmts stmts = {0};
    while (!check(p, TOKEN_RBRACE) && !check(p, TOKEN_EOF)) {
        Stmt *stmt = declaration(p);
        arena_da_append(&p->arena, &stmts, stmt);
    }

    consume(p, TOKEN_RBRACE, "Expect '}' after block");
    return stmts;
}

static Stmt *function(Parser *p, FnType type) {
    FnType prvFn = p->curFN;
    p->curFN = type;

    Token name = p->prv;
    consume(p, TOKEN_LPAREN, "Expect '(' after function name");

    Tokens params = {0};
    if (!check(p, TOKEN_RPAREN)) {
        do {
            if (params.cnt > 255) {
                errorAtCur(p, "Can't have more than 255 parameters");
            }
            consume(p, TOKEN_IDENTIFIER, "Expect parameter name");
            arena_da_append(&p->arena, &params, p->prv);
        } while (match(p, TOKEN_COMMA));
    }

    consume(p, TOKEN_RPAREN, "Expect ')' after parameters");
    consume(p, TOKEN_LBRACE, "Expect '{' before function body");

    Stmts body = block(p);

    Stmt *fn = newStmt(STMT_FUN, name, .params = params, .bodyf = body,
                       .fnType = type);
    p->curFN = prvFn;
    return fn;
}

static Stmt *method(Parser *p) {
    consume(p, TOKEN_IDENTIFIER, "Expect method name");
    Token name = p->prv;

    FnType type = FN_METHOD;
    if (p->prv.cnt == 4 && stringsEqual(name.lexeme, svLit("init"))) {
        type = FN_INIT;
    }
    return function(p, type);
}

static Stmt *classDecl(Parser *p) {
    ClassType prvCLS = p->curCLS;
    p->curCLS = CLS_CLASS;

    consume(p, TOKEN_IDENTIFIER, "Expect class name");
    Token name = p->prv;

    ExprIdent supercls = {0};
    if (match(p, TOKEN_LT)) {
        consume(p, TOKEN_IDENTIFIER, "Expect superclass name");

        supercls = (ExprIdent){p->scopeDepth, -1, p->prv};
        if (stringsEqual(supercls.name.lexeme, name.lexeme)) {
            error(p, "A class can't inherit from itself");
        }

        p->curCLS = CLS_SUBCLASS;
    }

    consume(p, TOKEN_RBRACE, "Expect '{' before class body");

    Stmts methods = {0};
    while (!check(p, TOKEN_RBRACE) && !check(p, TOKEN_EOF)) {
        Stmt *method_ = method(p);
        if (method_ != NULL) arena_da_append(&p->arena, &methods, method_);
    }

    consume(p, TOKEN_RBRACE, "Expect '}' after class body");
    p->curCLS = prvCLS;

    return newStmt(STMT_CLASS, name, .superClass = supercls,
                   .methods = methods);
}

static Stmt *funDecl(Parser *p) { return function(p, FN_FUNC); }

static Stmt *varDecl(Parser *p) {
    consume(p, TOKEN_IDENTIFIER, "Expect variable name");
    Token name = p->prv;

    Expr *init = NULL;
    if (match(p, TOKEN_EQ)) init = expression(p);

    consumeSemiColon(p, "variable declaration");

    return newStmt(STMT_VAR, name, .init = init);
}

static Stmt *declaration(Parser *p) {
    Stmt *stmt = NULL;
    if (match(p, TOKEN_CLASS)) {
        stmt = classDecl(p);
    } else if (match(p, TOKEN_FUN)) {
        stmt = funDecl(p);
    } else if (match(p, TOKEN_VAR)) {
        stmt = varDecl(p);
    } else {
        stmt = statement(p);
    }

    if (p->panicMode) synchronize(p);
    return stmt;
}

static Stmt *forIterStmt(Parser *p) {
    UNUSED(p);
    TODO("");
    return NULL;
}

static Stmt *forStmt(Parser *p) {
    consume(p, TOKEN_LPAREN, "Expect '(' after 'for'");

    // save parser in case it isn't a for iter stmt
    Arena_Mark mark = {0};
    Parser savedParser = saveParser(p, &mark);
    // parse for iter stmts
    Stmt *stmt = forIterStmt(p);
    if (stmt != NULL) return stmt;

    // restore parser if it wasn't a for iter stmt
    rewindParser(p, mark, &savedParser);

    // parse a normal for loop
    Stmt *init = NULL;
    if (match(p, TOKEN_SEMICOLON)) {
        // no initializer
    } else if (match(p, TOKEN_VAR)) {
        init = varDecl(p);
    } else {
        Expr *initExpr = expression(p);
        init = newStmt(STMT_EXPR, ((Token){0}), .expr = initExpr);
    }

    Expr *cond = NULL;
    if (!match(p, TOKEN_SEMICOLON)) cond = expression(p);
    consumeSemiColon(p, "loop condition");

    Expr *incr = NULL;
    if (!match(p, TOKEN_RPAREN)) incr = expression(p);
    consume(p, TOKEN_RPAREN, "Expect ')' after for clauses");

    p->loopDepth++;

    Stmt *body = statement(p);

    if (incr != NULL) {
        Stmts tmp = {0};
        arena_da_append(&p->arena, &tmp, body);
        Stmt *incrStmt = newStmt(STMT_EXPR, ((Token){0}), .expr = incr);
        arena_da_append(&p->arena, &tmp, incrStmt);

        body = newStmt(STMT_BLOCK, ((Token){0}), .block = tmp);
    }

    if (cond == NULL) {
        Token tok = {TOKEN_TRUE, .lexeme = svLit("true")};
        cond = newExpr(EXPR_LITERAL, tok);
    }
    body = newStmt(STMT_WHILE, ((Token){0}), .cond = cond, .bodyw = body);

    if (init != NULL) {
        Stmts tmp = {0};
        arena_da_append(&p->arena, &tmp, init);
        arena_da_append(&p->arena, &tmp, body);

        body = newStmt(STMT_BLOCK, ((Token){0}), .block = tmp);
    }

    p->loopDepth--;
    return body;
}

static Stmt *ifStmt(Parser *p) {
    Token tok = p->prv;

    consume(p, TOKEN_LPAREN, "Expect '(' after if");
    Expr *cond = expression(p);
    consume(p, TOKEN_RPAREN, "Expect ')' after condition");

    Stmt *then = statement(p);

    Stmt *elze = NULL;
    if (match(p, TOKEN_ELSE)) elze = statement(p);

    return newStmt(STMT_IF, tok, .cond = cond, .then = then, .elze = elze);
}

static Stmt *whileStmt(Parser *p) {
    Token tok = p->prv;
    consume(p, TOKEN_LPAREN, "Expect '(' after 'while'");
    Expr *cond = expression(p);
    consume(p, TOKEN_RPAREN, "Expect ')' after condition");

    p->loopDepth++;
    Stmt *body = statement(p);
    p->loopDepth--;
    return newStmt(STMT_WHILE, tok, .cond = cond, .bodyw = body);
}

static Stmt *statement(Parser *p) {
    if (match(p, TOKEN_PRINT)) {
        Token tok = p->prv;
        Expr *value = expression(p);
        consumeSemiColon(p, "value");
        return newStmt(STMT_PRINT, tok, .print = value);
    } else if (match(p, TOKEN_FOR)) {
        return forStmt(p);
    } else if (match(p, TOKEN_IF)) {
        return ifStmt(p);
    } else if (match(p, TOKEN_WHILE)) {
        return whileStmt(p);
    } else if (match(p, TOKEN_LBRACE)) {
        Token tok = p->prv;
        Stmts block_ = block(p);
        return newStmt(STMT_BLOCK, tok, .block = block_);
    } else if (match(p, TOKEN_RETURN)) {
        Token tok = p->prv;
        if (p->curFN == FN_NONE) error(p, "Can't return from top-level code");

        if (match(p, TOKEN_SEMICOLON)) {
            if (p->hadErr) return NULL;
            return newStmt(STMT_CONTROL, tok, .init = NULL);
        } else {
            if (p->curFN == FN_INIT) {
                error(p, "Can't return a value from an initializer");
            }

            Expr *init = expression(p);
            consumeSemiColon(p, "return value");
            return newStmt(STMT_CONTROL, tok, .init = init);
        }
    } else if (match(p, TOKEN_BREAK)) {
        if (p->loopDepth == 0) {
            error(p, "Can't use 'break' outside a loop");
            return NULL;
        }
        Token tok = p->prv;
        consumeSemiColon(p, "break");
        return newStmt(STMT_CONTROL, tok);
    } else if (match(p, TOKEN_CONTINUE)) {
        if (p->loopDepth == 0) {
            error(p, "Can't use 'continue' outside a loop");
            return NULL;
        }
        Token tok = p->prv;
        consumeSemiColon(p, "continue");
        return newStmt(STMT_CONTROL, tok);
    } else {
        Token tok = p->prv;
        Expr *value = expression(p);
        consumeSemiColon(p, "expression");
        return newStmt(STMT_EXPR, tok, .expr = value);
    }
}

bool parse(Parser *p, Stmts *stmts) {
    stmts->cnt = 0;
    advance(p);

    while (!match(p, TOKEN_EOF)) {
        Stmt *stmt = declaration(p);
        if (stmt != NULL) arena_da_append(&p->arena, stmts, stmt);
    }

    return !p->hadErr;
}

void initParser(Parser *p) { *p = (Parser){0}; }

void resetParser(Parser *p, const char *src) {
    Arena savedArena = p->arena;
    *p = (Parser){0};

    p->arena = savedArena;
    arena_reset(&p->arena);
    initLexer(&p->lexer, src);
}

void freeParser(Parser *p) { arena_free(&p->arena); }

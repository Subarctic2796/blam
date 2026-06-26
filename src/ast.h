#ifndef INCLUDE_SRC_AST_H_
#define INCLUDE_SRC_AST_H_

#include "common.h"
#include "token.h"

typedef struct Expr Expr;
typedef struct Stmt Stmt;

typedef enum {
    EXPR_ARRAY,
    EXPR_ASSIGN,
    EXPR_BINARY,
    EXPR_CALL,
    EXPR_GET,
    EXPR_GROUPING,
    EXPR_HASH,
    EXPR_IDENT,
    EXPR_IF,
    EXPR_INDEXED_GET,
    EXPR_INDEXED_SET,
    EXPR_LAMBDA,
    EXPR_LITERAL,
    EXPR_LOGICAL,
    EXPR_SET,
    EXPR_SUPER,
    EXPR_THIS,
    EXPR_UNARY,
} ExprType;

typedef struct {
    Expr **items;
    size_t cnt, cap;
} Exprs;

typedef struct {
    Token oper;
    Expr *value;
} ExprAssign;

typedef struct {
    Expr *lhs;
    Expr *rhs;
} ExprBinary;

typedef ExprBinary ExprLogical;

typedef struct {
    Expr *callee;
    Exprs args;
} ExprCall;

// TODO: add slicing
typedef struct {
    Expr *object;
    Expr *index;
} ExprIndexedGet;

typedef struct {
    Expr *object;
    Expr *value;
} ExprSet;

typedef struct {
    Expr *object;
    Expr *index;
    Expr *value;
} ExprIndexedSet;

typedef struct {
    int scope, index;
    Token name;
} ExprIdent;

typedef struct StmtFn StmtFn;
typedef struct StmtIf StmtIf;

typedef struct Expr {
    ExprType type;
    Token token;
    union {
        Exprs elements; // array or hash literal
        ExprAssign assign;
        ExprBinary binary;
        ExprLogical logical;
        ExprCall call;
        ExprIndexedGet indexedGet;
        ExprIndexedSet indexedSet;
        Expr *get;
        Expr *grouping;
        Stmt *lambda; // StmtFn
        Stmt *if_;    // StmtIf
        ExprSet set;
        Token method;
        Expr *right;
        ExprIdent ident;
    } as;
} Expr;

typedef struct {
} ExprOpts;

typedef enum {
    STMT_BLOCK,
    STMT_CLASS,
    STMT_CONTROL,
    STMT_EXPR,
    STMT_FUN,
    STMT_IF,
    STMT_PRINT,
    STMT_VAR,
    STMT_WHILE,
} StmtType;

typedef struct {
    Stmt **items;
    size_t cnt, cap;
} Stmts;

typedef struct {
    Stmts methods; // really []*StmtFn
    ExprIdent superClass;
} StmtClass;

typedef enum {
    FN_FUNC,
    FN_LAMBDA,
    FN_INIT,
    FN_METHOD,
    FN_STATIC,
} FnType;

typedef struct StmtFn {
    FnType type;
    Stmts body;
    Tokens params;
} StmtFn;

typedef struct StmtIf {
    Expr *cond;
    Stmt *then;
    Stmt *elze;
} StmtIf;

typedef struct {
    Expr *cond;
    Stmt *body;
} StmtWhile;

typedef struct Stmt {
    StmtType type;
    Token token;
    union {
        Stmts block;
        StmtClass klass;
        Expr *expr;
        StmtFn fun;
        StmtIf if_;
        Expr *print;
        Expr *value; // control
        Expr *init;  // var
        StmtWhile while_;
    } as;
} Stmt;

typedef struct {
} StmtOpts;

#define newExpr(t, ...) newExprOpts(t, (ExprOpts){__VA_ARGS__})
Expr *newExprOpts(ExprType, ExprOpts opts);

#define newStmt(t, ...) newStmtOpts(t, (StmtOpts){__VA_ARGS__})
Stmt *newStmtOpts(StmtType, StmtOpts opts);

void printExpr(const Expr *expr);
void freeExpr(Expr *expr);

void clearExprs(Exprs *exprs);
void freeExprs(Exprs *exprs);

void printStmt(const Stmt *stmt);
void freeStmt(Stmt *stmt);

void clearStmts(Stmts *stmts);
void freeStmts(Stmts *stmts);

#endif // INCLUDE_SRC_AST_H_

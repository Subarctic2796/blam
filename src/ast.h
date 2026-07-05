#ifndef INCLUDE_SRC_AST_H_
#define INCLUDE_SRC_AST_H_

#include "arena.h"
#include "common.h"
#include "token.h"

typedef enum {
    SCOPE_NONE,
    SCOPE_GLOBAL,
    SCOPE_LOCAL,
    SCOPE_UPVALUE,
    __SCOPE_CNT,
} ScopeType;

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
    ScopeType scope;
    int index;
    Expr *value;
    Token oper;
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
    ScopeType scope;
    bool isLocal;
    int index;
    Token name;
} ExprIdent;

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
        Expr *group;
        Stmt *lambda; // StmtFn
        Stmt *if_;    // StmtIf
        ExprSet set;
        Token method;
        Expr *right;
        ExprIdent ident;
    } as;
} Expr;

typedef struct {
    ScopeType scope;
    bool isLocal;
    int idx;
    union {
        Expr *lhs;
        Expr *object;
        Expr *callee;
        Expr *get;
        Expr *group;
    };
    union {
        Expr *rhs;
        Expr *index;
    };
    Expr *value;
    union {
        Stmt *lambda;
        Stmt *if_;
    };
    union {
        Token name;
        Token opr;
        Token method;
    };
    union {
        Exprs elements;
        Exprs args;
    };
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
    STMT_FOR_IN,
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
    FN_NONE,
    FN_SCRIPT,
    FN_FUNC,
    FN_LAMBDA,
    FN_INIT,
    FN_METHOD,
    FN_STATIC,
    __FN_CNT,
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

typedef struct {
    Expr *iter;
    ExprIdent index;
    ExprIdent name;
    Stmt *body;
} StmtForIn;

typedef struct {
    Expr *init;
    ExprIdent name;
} StmtVar;

typedef struct Stmt {
    StmtType type;
    Token token;
    union {
        Stmts block;
        StmtClass klass;
        StmtFn fun;
        StmtIf if_;
        StmtWhile while_;
        StmtForIn forIn;
        StmtVar var;
        Expr *expr;
        Expr *print;
        Expr *value; // control
    } as;
} Stmt;

typedef struct {
    FnType fnType;
    union {
        Expr *cond;
        Expr *expr;
        Expr *print;
        Expr *value;
        Expr *init;
        Expr *iter;
    };
    Stmt *then;
    union {
        Stmt *elze;
        Stmt *bodyw;
    };
    union {
        Stmts block;
        Stmts methods;
        Stmts bodyf;
    };
    Tokens params;
    union {
        ExprIdent superClass;
        ExprIdent index;
    };
    ExprIdent name;
} StmtOpts;

#define newExpr(ty, tk, ...)                                                   \
    newExprOpts(&p->arena, ty, tk, (ExprOpts){__VA_ARGS__})
Expr *newExprOpts(Arena *a, ExprType type, Token tok, ExprOpts opts);

#define newStmt(ty, tk, ...)                                                   \
    newStmtOpts(&p->arena, ty, tk, (StmtOpts){__VA_ARGS__})
Stmt *newStmtOpts(Arena *a, StmtType type, Token tok, StmtOpts opts);

void printExpr(const Expr *expr);
void printStmt(const Stmt *stmt);

static inline const char *FnTypeStr(const FnType t) {
    static const char *strings[] = {
        "FN_NONE",   // FN_NONE
        "FN_SCRIPT", // FN_SCRIPT
        "FN_FUNC",   // FN_FUNC
        "FN_LAMBDA", // FN_LAMBDA
        "FN_INIT",   // FN_INIT
        "FN_METHOD", // FN_METHOD
        "FN_STATIC", // FN_STATIC
    };
    static_assert(ARRAY_LEN(strings) == __FN_CNT, "number of FnTypes changed");
    return strings[t];
}

static inline const char *ScopeTypeStr(const ScopeType t) {
    static const char *strings[] = {
        "SCOPE_NONE",    // SCOPE_NONE
        "SCOPE_GLOBAL",  // SCOPE_GLOBAL
        "SCOPE_LOCAL",   // SCOPE_LOCAL
        "SCOPE_UPVALUE", // SCOPE_UPVALUE
    };
    static_assert(ARRAY_LEN(strings) == __SCOPE_CNT,
                  "number of ScopeTypes changed");
    return strings[t];
}

#endif // INCLUDE_SRC_AST_H_

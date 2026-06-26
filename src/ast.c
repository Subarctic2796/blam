#include "ast.h"
#include "common.h"
#include "token.h"

Expr *newExprOpts(ExprType, ExprOpts opts);

Stmt *newStmtOpts(StmtType, StmtOpts opts);

void printExpr(const Expr *expr) {
    switch (expr->type) {
    case EXPR_ARRAY:
    case EXPR_ASSIGN:
    case EXPR_BINARY:
    case EXPR_CALL:
    case EXPR_GET:
    case EXPR_GROUPING:
    case EXPR_HASH:
    case EXPR_IDENT:
    case EXPR_IF:
    case EXPR_INDEXED_GET:
    case EXPR_INDEXED_SET:
    case EXPR_LAMBDA:
    case EXPR_LITERAL:
    case EXPR_LOGICAL:
    case EXPR_SET:
    case EXPR_SUPER:
    case EXPR_THIS:
    case EXPR_UNARY:       TODO(""); break;
    }
}

void freeExpr(Expr *expr) {
    switch (expr->type) {
    case EXPR_IDENT:
    case EXPR_LITERAL:
    case EXPR_SUPER:
    case EXPR_THIS:     break;
    case EXPR_HASH:
    case EXPR_ARRAY:    freeExprs(&expr->as.elements); break;
    case EXPR_ASSIGN:   freeExpr(expr->as.assign.value); break;
    case EXPR_GROUPING: freeExpr(expr->as.grouping); break;
    case EXPR_GET:      freeExpr(expr->as.get); break;
    case EXPR_UNARY:    freeExpr(expr->as.right); break;
    case EXPR_LAMBDA:   freeStmt(expr->as.lambda); break;
    case EXPR_IF:       freeStmt(expr->as.if_); break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:   {
        ExprBinary bin = expr->as.binary;
        freeExpr(bin.lhs);
        freeExpr(bin.rhs);
    } break;
    case EXPR_CALL: {
        ExprCall call = expr->as.call;
        freeExpr(call.callee);
        freeExprs(&call.args);
    } break;
    case EXPR_INDEXED_GET: {
        ExprIndexedGet get = expr->as.indexedGet;
        freeExpr(get.object);
        freeExpr(get.index);
    } break;
    case EXPR_INDEXED_SET: {
        ExprIndexedSet set = expr->as.indexedSet;
        freeExpr(set.object);
        freeExpr(set.index);
        freeExpr(set.value);
    } break;
    case EXPR_SET: {
        ExprSet set = expr->as.set;
        freeExpr(set.object);
        freeExpr(set.value);
    } break;
    }
    free(expr);
    expr = NULL;
}

void clearExprs(Exprs *exprs) {
    for (size_t i = 0; i < exprs->cnt; i++) {
        freeExpr(exprs->items[i]);
    }
    exprs->cnt = 0;
}

void freeExprs(Exprs *exprs) {
    clearExprs(exprs);
    da_free(*exprs);
    *exprs = (Exprs){0};
}

void printStmt(const Stmt *stmt) {
    switch (stmt->type) {
    case STMT_BLOCK:
    case STMT_CLASS:
    case STMT_CONTROL:
    case STMT_EXPR:
    case STMT_FUN:
    case STMT_IF:
    case STMT_PRINT:
    case STMT_VAR:
    case STMT_WHILE:   TODO(""); break;
    }
}

void freeStmt(Stmt *stmt) {
    switch (stmt->type) {
    case STMT_BLOCK: freeStmts(&stmt->as.block); break;
    case STMT_EXPR:  freeExpr(stmt->as.expr); break;
    case STMT_PRINT: freeExpr(stmt->as.print); break;
    case STMT_VAR:   freeExpr(stmt->as.init); break;
    case STMT_CLASS: {
        StmtClass klass = stmt->as.klass;
        freeStmts(&klass.methods);
    } break;
    case STMT_CONTROL: {
        if (stmt->token.type == TOKEN_RETURN) {
            Expr *value = stmt->as.value;
            if (value != NULL) freeExpr(value);
        }
    } break;
    case STMT_FUN: {
        StmtFn fun = stmt->as.fun;
        da_free(fun.params);
        freeStmts(&fun.body);
    } break;
    case STMT_IF: {
        StmtIf if_ = stmt->as.if_;
        freeExpr(if_.cond);
        freeStmt(if_.then);
        if (if_.elze != NULL) freeStmt(if_.elze);
    } break;
    case STMT_WHILE: {
        StmtWhile while_ = stmt->as.while_;
        freeExpr(while_.cond);
        freeStmt(while_.body);
    } break;
    }
    free(stmt);
    stmt = NULL;
}

void clearStmts(Stmts *stmts) {
    for (size_t i = 0; i < stmts->cnt; i++) {
        freeStmt(stmts->items[i]);
    }
    stmts->cnt = 0;
}

void freeStmts(Stmts *stmts) {
    clearStmts(stmts);
    da_free(*stmts);
    *stmts = (Stmts){0};
}

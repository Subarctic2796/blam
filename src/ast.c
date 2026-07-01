#include "ast.h"
#include "common.h"
#include "token.h"

static inline Expr *allocExpr(Arena *a, ExprType type, Token tok) {
    Expr *ret = (Expr *)arena_alloc(a, sizeof(*ret));
    *ret = (Expr){0};
    ret->token = tok;
    ret->type = type;
    return ret;
}

Expr *newExprOpts(Arena *a, ExprType type, Token tok, ExprOpts opts) {
    Expr *ret = allocExpr(a, type, tok);
    switch (type) {
    case EXPR_HASH:
    case EXPR_ARRAY:    ret->as.elements = opts.elements; break;
    case EXPR_SUPER:    ret->as.method = opts.method; break;
    case EXPR_SET:      ret->as.set = (ExprSet){opts.object, opts.value}; break;
    case EXPR_GET:      ret->as.get = opts.get; break;
    case EXPR_GROUPING: ret->as.group = opts.group; break;
    case EXPR_ASSIGN:
        ret->as.assign =
            (ExprAssign){opts.scope, opts.idx, opts.value, opts.opr};
        break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:  ret->as.binary = (ExprBinary){opts.lhs, opts.rhs}; break;
    case EXPR_CALL:    ret->as.call = (ExprCall){opts.callee, opts.args}; break;
    case EXPR_UNARY:   ret->as.right = opts.rhs; break;
    case EXPR_IF:      ret->as.if_ = opts.if_; break;
    case EXPR_LAMBDA:  ret->as.lambda = opts.lambda; break;
    case EXPR_IDENT:
        ret->as.ident = (ExprIdent){opts.scope, opts.idx, opts.name};
        break;
    case EXPR_INDEXED_GET:
        ret->as.indexedGet = (ExprIndexedGet){opts.object, opts.index};
        break;
    case EXPR_INDEXED_SET:
        ret->as.indexedSet =
            (ExprIndexedSet){opts.object, opts.index, opts.value};
        break;
    case EXPR_THIS:
    case EXPR_LITERAL: break;
    }
    return ret;
}

static inline Stmt *allocStmt(Arena *a, StmtType type, Token tok) {
    Stmt *ret = (Stmt *)arena_alloc(a, sizeof(*ret));
    *ret = (Stmt){0};
    ret->token = tok;
    ret->type = type;
    return ret;
}

Stmt *newStmtOpts(Arena *a, StmtType type, Token tok, StmtOpts opts) {
    Stmt *ret = allocStmt(a, type, tok);
    switch (type) {
    case STMT_BLOCK:   ret->as.block = opts.block; break;
    case STMT_CONTROL: ret->as.value = opts.value; break;
    case STMT_EXPR:    ret->as.expr = opts.expr; break;
    case STMT_PRINT:   ret->as.print = opts.print; break;
    case STMT_VAR:     ret->as.init = opts.init; break;
    case STMT_WHILE:   ret->as.while_ = (StmtWhile){opts.cond, opts.bodyw}; break;
    case STMT_CLASS:
        ret->as.klass = (StmtClass){opts.methods, opts.superClass};
        break;
    case STMT_FUN:
        ret->as.fun = (StmtFn){opts.fnType, opts.bodyf, opts.params};
        break;
    case STMT_IF:
        ret->as.if_ = (StmtIf){opts.cond, opts.then, opts.elze};
        break;
    case STMT_FOR_IN:
        ret->as.forIn =
            (StmtForIn){opts.iter, opts.index, opts.name, opts.bodyw};
        break;
    }
    return ret;
}

void printExpr(const Expr *expr) {
    Token tok = expr->token;
    switch (expr->type) {
    case EXPR_ARRAY: {
        Exprs elems = expr->as.elements;
        puts("([");
        for (size_t i = 0; i < elems.cnt; i++) {
            printf("    ");
            printExpr(elems.items[i]);
            putchar(',');
        }
        printf("])");
    } break;
    case EXPR_ASSIGN: {
        ExprAssign assign = expr->as.assign;
        printf("(%.*s %.*s ", (int)assign.oper.cnt, assign.oper.items,
               (int)tok.cnt, tok.items);
        printExpr(assign.value);
        putchar(')');
    } break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:  {
        ExprBinary bin = expr->as.binary;
        printf("(%.*s ", (int)tok.cnt, tok.items);
        printExpr(bin.lhs);
        putchar(' ');
        printExpr(bin.rhs);
        putchar(')');
    } break;
    case EXPR_CALL: {
        ExprCall call = expr->as.call;
        printf("(call ");
        printExpr(call.callee);
        for (size_t i = 0; i < call.args.cnt; i++) {
            Expr *arg = call.args.items[i];
            putchar(' ');
            printExpr(arg);
        }
        putchar(')');
    } break;
    case EXPR_GET: {
        printf("(. ");
        printExpr(expr->as.get);
        printf(" %.*s)", (int)tok.cnt, tok.items);
    } break;
    case EXPR_GROUPING: {
        printf("(group ");
        printExpr(expr->as.group);
        putchar(')');
    } break;
    case EXPR_HASH: {
        Exprs elems = expr->as.elements;
        puts("({");
        for (size_t i = 0; i < elems.cnt; i += 2) {
            printf("    ");
            printExpr(elems.items[i]);
            printf(": ");
            printExpr(elems.items[i + 1]);
            puts(",");
        }
        printf("})");
    } break;
    case EXPR_UNARY: {
        printf("(%.*s ", (int)tok.cnt, tok.items);
        printExpr(expr->as.right);
        putchar(')');
    } break;
    case EXPR_LITERAL:
    case EXPR_IDENT:       printf("%.*s", (int)tok.cnt, tok.items); break;
    case EXPR_THIS:        printf("(this)"); break;
    case EXPR_IF:          printStmt(expr->as.if_); break;
    case EXPR_LAMBDA:      printStmt(expr->as.lambda); break;
    case EXPR_INDEXED_GET: {
        ExprIndexedGet get = expr->as.indexedGet;
        putchar('(');
        printExpr(get.object);
        putchar('[');
        printExpr(get.index);
        printf("])");
    } break;
    case EXPR_INDEXED_SET: {
        ExprIndexedSet set = expr->as.indexedSet;
        putchar('(');
        printExpr(set.object);
        putchar('[');
        printExpr(set.index);
        printf("] = ");
        printExpr(set.value);
        putchar(')');
    } break;
    case EXPR_SET: {
        ExprSet set = expr->as.set;
        printf("(= ");
        printExpr(set.object);
        printf(" %.*s ", (int)tok.cnt, tok.items);
        printExpr(set.value);
        putchar(')');
    } break;
    case EXPR_SUPER: {
        Token method = expr->as.method;
        printf("(super %.*s)", (int)method.cnt, method.items);
    } break;
    }
}

void printStmt(const Stmt *stmt) {
    Token tok = stmt->token;
    switch (stmt->type) {
    case STMT_BLOCK: {
        Stmts block = stmt->as.block;
        printf("(block ");
        for (size_t i = 0; i < block.cnt; i++) {
            printStmt(block.items[i]);
            if (i != block.cnt - 1) putchar(' ');
        }
        putchar(')');
    } break;
    case STMT_CLASS: {
        StmtClass klass = stmt->as.klass;
        printf("(class %.*s", (int)tok.cnt, tok.items);
        if (klass.superClass.name.items != NULL) {
            Token name = klass.superClass.name;
            printf(" < %.*s", (int)name.cnt, name.items);
        }
        Stmts methods = klass.methods;
        for (size_t i = 0; i < methods.cnt; i++) {
            putchar(' ');
            printStmt(methods.items[i]);
        }
        putchar(')');
    } break;
    case STMT_EXPR: {
        printf("(; ");
        printExpr(stmt->as.expr);
        putchar(')');
    } break;
    case STMT_CONTROL: {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        switch (tok.type) {
        case TOKEN_RETURN: {
            printf("(return");
            if (stmt->as.value != NULL) {
                putchar(' ');
                printExpr(stmt->as.value);
            }
            putchar(')');
        } break;
        case TOKEN_BREAK:    printf("(break)"); break;
        case TOKEN_CONTINUE: printf("(continue)"); break;
        default:
            UNREACHABLE("control type is not return, break, or continue");
            break;
        }
#pragma GCC diagnostic pop
    } break;
    case STMT_FUN: {
        StmtFn fun = stmt->as.fun;
        if (fun.type == FN_LAMBDA) {
            printf("(fun(");
        } else {
            printf("(fun %.*s(", (int)tok.cnt, tok.items);
        }
        for (size_t i = 0; i < fun.params.cnt; i++) {
            if (i != 0) {
                putchar(' ');
            }
            Token param = fun.params.items[i];
            printf("%.*s", (int)param.cnt, param.items);
        }
        printf(") ");
        for (size_t i = 0; i < fun.body.cnt; i++) {
            printStmt(fun.body.items[i]);
        }
        putchar(')');
    } break;
    case STMT_IF: {
        StmtIf if_ = stmt->as.if_;
        if (if_.elze != NULL) {
            printf("(if ");
            printExpr(if_.cond);
            putchar(' ');
            printStmt(if_.then);
        } else {
            printf("(if-else ");
            printExpr(if_.cond);
            putchar(' ');
            printStmt(if_.then);
            putchar(' ');
            printStmt(if_.elze);
        }
        putchar(')');
    } break;
    case STMT_PRINT: {
        printf("(print ");
        printExpr(stmt->as.print);
        putchar(')');
    } break;
    case STMT_VAR: {
        printf("(var %.*s", (int)tok.cnt, tok.items);
        if (stmt->as.init != NULL) {
            printf(" = ");
            printExpr(stmt->as.init);
        }
        putchar(')');
    } break;
    case STMT_WHILE: {
        StmtWhile while_ = stmt->as.while_;
        printf("(while ");
        printExpr(while_.cond);
        putchar(' ');
        printStmt(while_.body);
        putchar(')');
    } break;
    case STMT_FOR_IN: {
        StmtForIn forIn = stmt->as.forIn;
        ExprIdent index = forIn.index;
        ExprIdent name = forIn.name;
        printf("(for-in ");
        if (index.name.items != NULL) {
            printf("%.*s ", (int)index.name.cnt, index.name.items);
        }
        printf("%.*s ", (int)name.name.cnt, name.name.items);
        printExpr(forIn.iter);
        putchar(' ');
        printStmt(forIn.body);
        putchar(')');
    } break;
    }
}

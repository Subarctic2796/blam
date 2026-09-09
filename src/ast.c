#include <stdarg.h>

#include "arena.h"
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
        ret->as.assign = (ExprAssign){opts.scope, opts.isLocal, opts.idx,
                                      opts.depth, opts.value,   opts.opr};
        break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:  ret->as.binary = (ExprBinary){opts.lhs, opts.rhs}; break;
    case EXPR_CALL:    ret->as.call = (ExprCall){opts.callee, opts.args}; break;
    case EXPR_UNARY:   ret->as.right = opts.rhs; break;
    case EXPR_IF:      ret->as.if_ = opts.if_; break;
    case EXPR_LAMBDA:  ret->as.lambda = opts.lambda; break;
    case EXPR_IDENT:
        ret->as.ident = (ExprIdent){opts.scope, opts.isLocal, opts.idx,
                                    opts.depth, opts.name};
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
    case STMT_VAR:     ret->as.var = (StmtVar){opts.init, opts.name}; break;
    case STMT_WHILE:   ret->as.while_ = (StmtWhile){opts.cond, opts.bodyw}; break;
    case STMT_CLASS:
        ret->as.klass = (StmtClass){opts.methods, opts.scope, opts.superClass};
        break;
    case STMT_FUN:
        ret->as.fun =
            (StmtFn){opts.fnType, opts.upvaluesCnt, opts.bodyf, opts.params};
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
        if (elems.cnt == 0) {
            printf("([])");
            return;
        }

        puts("([");
        for (size_t i = 0; i < elems.cnt; i++) {
            printf("    ");
            printExpr(elems.items[i]);
            puts(",");
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
        if (elems.cnt == 0) {
            printf("({})");
            return;
        }

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
    case EXPR_LITERAL: printf("%.*s", (int)tok.cnt, tok.items); break;
    case EXPR_IDENT:   {
        ExprIdent ident = expr->as.ident;
        printf("%.*s[%s:%d:%d]", (int)tok.cnt, tok.items,
               ScopeTypeStr(ident.scope), ident.depth, ident.index);
    } break;
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
            if (i != 0) putchar(' ');
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
        ExprIdent ident = stmt->as.var.name;
        printf("(var %.*s[%s:%d:%d]", (int)tok.cnt, tok.items,
               ScopeTypeStr(ident.scope), ident.depth, ident.index);
        if (stmt->as.var.init != NULL) {
            printf(" = ");
            printExpr(stmt->as.var.init);
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

static inline void astPrinterAdd(const AstPrinter *ap, const char *v) {
    printf("%*s%s", ap->depth * 4, " ", v);
}

static inline void astPrinterAddLine(const AstPrinter *ap, const char *line) {
    printf("%*s%s\n", ap->depth * 4, " ", line);
}

static inline void astPrinterAddLinef(const AstPrinter *ap, const char *format,
                                      ...) {
    printf("%*s", ap->depth * 4, " ");
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    puts("");
}

static void astPrinterPrintExpr(AstPrinter *ap, const Expr *expr);
static void astPrinterPrintStmt(AstPrinter *ap, const Stmt *stmt);

static inline void astPrinterStart(AstPrinter *ap, const char *label) {
    astPrinterAddLinef(ap, "%s(", label);
    ap->depth++;
}

static inline void astPrinterEnd(AstPrinter *ap) {
    ap->depth--;
    astPrinterAddLine(ap, ")");
}

static void astPrinterPrintExpr(AstPrinter *ap, const Expr *expr) {
    Token tok = expr->token;
    switch (expr->type) {
    case EXPR_THIS:  astPrinterAddLine(ap, "This"); break;
    case EXPR_ARRAY: {
        Exprs arr = expr->as.elements;
        astPrinterStart(ap, "Array");
        for (size_t i = 0; i < arr.cnt; i++) {
            astPrinterPrintExpr(ap, arr.items[i]);
        }
        astPrinterEnd(ap);
    } break;
    case EXPR_HASH: {
        Exprs arr = expr->as.elements;
        astPrinterStart(ap, "Hash");
        for (size_t i = 0; i < arr.cnt; i += 2) {
            astPrinterAdd(ap, "key=");
            astPrinterPrintExpr(ap, arr.items[i]);
            astPrinterAdd(ap, "value=");
            astPrinterPrintExpr(ap, arr.items[i + 1]);
        }
        astPrinterEnd(ap);
    } break;
    case EXPR_ASSIGN: {
        ExprAssign assign = expr->as.assign;
        astPrinterStart(ap, "Assign");

        astPrinterAddLinef(ap, "op='%.*s',", (int)assign.oper.cnt,
                           assign.oper.items);
        astPrinterAddLinef(ap, "ident=%.*s,", (int)tok.cnt, tok.items);
        astPrinterAdd(ap, "value=");
        astPrinterPrintExpr(ap, assign.value);
        astPrinterEnd(ap);
    } break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:  {
        ExprBinary bin = expr->as.binary;

        astPrinterStart(ap, "Binary");

        astPrinterAdd(ap, "left=");
        astPrinterPrintExpr(ap, bin.lhs);
        astPrinterAddLine(ap, ",");
        astPrinterAddLinef(ap, "op='%.*s',", (int)tok.cnt, tok.items);
        astPrinterAdd(ap, "right=");
        astPrinterPrintExpr(ap, bin.rhs);

        astPrinterEnd(ap);
    } break;
    case EXPR_UNARY: {
        astPrinterStart(ap, "Unary");
        astPrinterAddLinef(ap, "op=%*s,", (int)tok.cnt, tok.items);
        astPrinterAdd(ap, "operand=");
        astPrinterPrintExpr(ap, expr->as.right);
        astPrinterEnd(ap);
    } break;
    case EXPR_CALL: {
        ExprCall call = expr->as.call;
        astPrinterStart(ap, "Call");

        astPrinterAdd(ap, "Callee=");
        astPrinterPrintExpr(ap, call.callee);

        astPrinterStart(ap, "args=");
        for (size_t i = 0; i < call.args.cnt; i++) {
            astPrinterPrintExpr(ap, call.args.items[i]);
        }
        astPrinterEnd(ap);

        astPrinterEnd(ap);
    } break;
    case EXPR_LITERAL:
        astPrinterAddLinef(ap, "Literal(%.*s)", (int)tok.cnt, tok.items);
        break;
    case EXPR_GROUPING: {
        astPrinterStart(ap, "Group");
        astPrinterPrintExpr(ap, expr->as.group);
        astPrinterEnd(ap);
    } break;
    case EXPR_IDENT: {
        Token ident = expr->as.ident.name;
        astPrinterAddLinef(ap, "Ident(%.*s)", (int)ident.cnt, ident.items);
    } break;
    case EXPR_SUPER: {
        Token method = expr->as.method;
        astPrinterAddLinef(ap, "Super(%.*s)", (int)method.cnt, method.items);
    } break;
    case EXPR_GET: {
        astPrinterStart(ap, "Get");
        astPrinterAdd(ap, "object=");
        astPrinterPrintExpr(ap, expr->as.get);
        astPrinterEnd(ap);
    } break;
    case EXPR_SET: {
        ExprSet set = expr->as.set;
        astPrinterStart(ap, "Set");

        astPrinterAdd(ap, "object=");
        astPrinterPrintExpr(ap, set.object);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "value=");
        astPrinterPrintExpr(ap, set.value);

        astPrinterEnd(ap);
    } break;
    case EXPR_INDEXED_GET: {
        ExprIndexedGet get = expr->as.indexedGet;
        astPrinterStart(ap, "IndexedGet");

        astPrinterAdd(ap, "object=");
        astPrinterPrintExpr(ap, get.object);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "index=");
        astPrinterPrintExpr(ap, get.index);

        astPrinterEnd(ap);
    } break;
    case EXPR_INDEXED_SET: {
        ExprIndexedSet set = expr->as.indexedSet;
        astPrinterStart(ap, "IndexedSet");
        astPrinterAdd(ap, "object=");
        astPrinterPrintExpr(ap, set.object);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "index=");
        astPrinterPrintExpr(ap, set.index);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "value=");
        astPrinterPrintExpr(ap, set.value);
        astPrinterEnd(ap);
    } break;
    case EXPR_LAMBDA: {
        StmtFn fn = expr->as.lambda->as.fun;
        astPrinterStart(ap, "Lambda");

        astPrinterStart(ap, "params=");
        for (size_t i = 0; i < fn.params.cnt; i++) {
            Token param = fn.params.items[i];
            astPrinterAddLinef(ap, "%.*s,", (int)param.cnt, param.items);
        }
        astPrinterEnd(ap);

        astPrinterAddLine(ap, "body=");
        for (size_t i = 0; i < fn.body.cnt; i++) {
            astPrinterPrintStmt(ap, fn.body.items[i]);
        }

        astPrinterEnd(ap);
    } break;
    case EXPR_IF: {
        StmtIf if_ = expr->as.if_->as.if_;
        astPrinterStart(ap, "If-Expr");

        astPrinterAdd(ap, "Cond=");
        astPrinterPrintExpr(ap, if_.cond);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "Then=");
        astPrinterPrintStmt(ap, if_.then);

        if (if_.elze != NULL) {
            astPrinterAddLine(ap, ",");
            astPrinterAdd(ap, "Else=");
            astPrinterPrintStmt(ap, if_.elze);
        }
        astPrinterEnd(ap);
    } break;
    }
}

static void astPrinterPrintStmt(AstPrinter *ap, const Stmt *stmt) {
    Token tok = stmt->token;
    switch (stmt->type) {
    case STMT_BLOCK: {
        astPrinterStart(ap, "Block");

        Stmts block = stmt->as.block;
        for (size_t i = 0; i < block.cnt; i++) {
            astPrinterPrintStmt(ap, block.items[i]);
        }

        astPrinterEnd(ap);
    } break;
    case STMT_CLASS: {
        StmtClass klass = stmt->as.klass;

        astPrinterStart(ap, "Class");

        if (klass.superClass.scope != SCOPE_NONE) {
            Token sc = klass.superClass.name;
            astPrinterAddLinef(ap, "superclass=\"%.*s\",", (int)sc.cnt,
                               sc.items);
        } else {
            astPrinterAddLine(ap, "superclass=\"\",");
        }
        astPrinterAddLinef(ap, "name=\"%.*s\",", (int)tok.cnt, tok.items);

        astPrinterStart(ap, "methods=");
        Stmts methods = klass.methods;
        for (size_t i = 0; i < methods.cnt; i++) {
            astPrinterPrintStmt(ap, methods.items[i]);
        }
        astPrinterEnd(ap);

        astPrinterEnd(ap);
    } break;
    case STMT_CONTROL: {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        switch (tok.type) {
        case TOKEN_RETURN: {
            if (stmt->as.value != NULL) {
                astPrinterStart(ap, "Return");
                astPrinterPrintExpr(ap, stmt->as.value);
                astPrinterEnd(ap);
            } else {
                astPrinterAddLine(ap, "Return()");
            }
        } break;
        case TOKEN_BREAK:    astPrinterAddLine(ap, "Break"); break;
        case TOKEN_CONTINUE: astPrinterAddLine(ap, "Continue"); break;
        default:
            UNREACHABLE("control type is not return, break, or continue");
            break;
        }
#pragma GCC diagnostic pop
    } break;
    case STMT_EXPR: {
        astPrinterStart(ap, "Expr Stmt");
        astPrinterPrintExpr(ap, stmt->as.expr);
        astPrinterEnd(ap);
    } break;
    case STMT_FUN: {
        StmtFn fn = stmt->as.fun;
        astPrinterStart(ap, "Func");

        astPrinterAddLinef(ap, "name=\"%.*s\",", (int)tok.cnt, tok.items);

        astPrinterStart(ap, "params=");
        for (size_t i = 0; i < fn.params.cnt; i++) {
            Token param = fn.params.items[i];
            astPrinterAddLinef(ap, "%.*s,", (int)param.cnt, param.items);
        }
        astPrinterEnd(ap);

        astPrinterAddLine(ap, "body=");
        for (size_t i = 0; i < fn.body.cnt; i++) {
            astPrinterPrintStmt(ap, fn.body.items[i]);
        }

        astPrinterEnd(ap);
    } break;
    case STMT_IF: {
        StmtIf if_ = stmt->as.if_;
        astPrinterStart(ap, "If");

        astPrinterAdd(ap, "Cond=");
        astPrinterPrintExpr(ap, if_.cond);
        astPrinterAddLine(ap, ",");

        astPrinterAdd(ap, "Then=");
        astPrinterPrintStmt(ap, if_.then);

        if (if_.elze != NULL) {
            astPrinterAddLine(ap, ",");
            astPrinterAdd(ap, "Else=");
            astPrinterPrintStmt(ap, if_.elze);
        }
        astPrinterEnd(ap);
    } break;
    case STMT_PRINT: {
        astPrinterStart(ap, "Print");
        astPrinterPrintExpr(ap, stmt->as.print);
        astPrinterEnd(ap);
    } break;
    case STMT_VAR: {
        StmtVar var = stmt->as.var;

        astPrinterStart(ap, "Var");
        Token name = var.name.name;
        astPrinterAddLinef(ap, "name=%.*s,", (int)name.cnt, name.items);
        if (var.init != NULL) {
            astPrinterAdd(ap, "init=");
            astPrinterPrintExpr(ap, var.init);
        }
        astPrinterEnd(ap);
    } break;
    case STMT_WHILE: {
        StmtWhile while_ = stmt->as.while_;
        astPrinterStart(ap, "While");

        astPrinterAdd(ap, "Cond=");
        astPrinterPrintExpr(ap, while_.cond);

        astPrinterAdd(ap, "Body=");
        astPrinterPrintStmt(ap, while_.body);

        astPrinterEnd(ap);
    } break;
    case STMT_FOR_IN: {
        StmtForIn forin = stmt->as.forIn;
        astPrinterStart(ap, "ForIn");

        if (forin.index.scope != SCOPE_NONE) {
            Token index = forin.index.name;
            astPrinterAddLinef(ap, "Index='%.*s',", (int)index.cnt,
                               index.items);
        }

        Token name = forin.name.name;
        astPrinterAddLinef(ap, "Name='%.*s',", (int)name.cnt, name.items);

        astPrinterAdd(ap, "Iter=");
        astPrinterPrintExpr(ap, forin.iter);

        astPrinterAddLine(ap, ",");
        astPrinterAdd(ap, "Body=");
        astPrinterPrintStmt(ap, forin.body);

        astPrinterEnd(ap);
    } break;
    }
}

void astPrinterPrint(AstPrinter *ap) {
    astPrinterStart(ap, "Program");
    for (size_t i = 0; i < ap->prog.cnt; i++) {
        astPrinterPrintStmt(ap, ap->prog.items[i]);
    }
    astPrinterEnd(ap);
}

static const string RHS_INDENT = {NULL, ~0};
static const string LHS_INDENT = {NULL, ~1};

// name must end with a '('
#define prettyPrintStart2(a, lines, name)                                      \
    do {                                                                       \
        arena_da_append(a, lines, strLit(name));                               \
        arena_da_append(a, lines, RHS_INDENT);                                 \
    } while (0)

#define prettyPrintEnd2(a, lines)                                              \
    do {                                                                       \
        arena_da_append(a, lines, LHS_INDENT);                                 \
        arena_da_append(a, lines, strLit(")"));                                \
    } while (0)

static void prettyPrintExpr2(Arena *a, strings *lines, const Expr *expr);
static void prettyPrintStmt2(Arena *a, strings *lines, const Stmt *stmt);

static void prettyPrintExpr2(Arena *a, strings *lines, const Expr *expr) {
    Token tok = expr->token;
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

static void prettyPrintStmt2(Arena *a, strings *lines, const Stmt *stmt) {
    Token tok = stmt->token;
    switch (stmt->type) {
    case STMT_EXPR: prettyPrintExpr2(a, lines, stmt->as.expr); break;
    case STMT_VAR:  {
        prettyPrintStart2(a, lines, "VAR(");

        prettyPrintEnd2(a, lines);
    } break;
    case STMT_BLOCK:
    case STMT_CLASS:
    case STMT_CONTROL:
    case STMT_FUN:
    case STMT_IF:
    case STMT_PRINT:
    case STMT_WHILE:
    case STMT_FOR_IN:  TODO(""); break;
    }
}

static inline bool printWithTabs(const string line, int depth) {
    if (line.cnt != 0) {
        switch (line.items[line.cnt - 1]) {
        case '(':
        case ')':
        case ',':
            printf("%*s%.*s\n", depth, "    ", (int)line.cnt, line.items);
            return true;
        }
    }
    printf("%*s%.*s", depth, "    ", (int)line.cnt, line.items);
    return false;
}

void astPrettyPrint2(const Stmts prog) {
    Arena arena = {0};
    strings lines = {0};

    prettyPrintStart2(&arena, &lines, "Program(");
    for (size_t i = 0; i < prog.cnt; i++) {
        prettyPrintStmt2(&arena, &lines, prog.items[i]);
    }
    prettyPrintEnd2(&arena, &lines);

    int depth = 0;
    bool prvEOL = true;
    for (size_t i = 0; i < lines.cnt; i++) {
        string line = lines.items[i];
        if (line.cnt == RHS_INDENT.cnt) {
            depth++;
            continue;
        } else if (line.cnt == LHS_INDENT.cnt) {
            depth--;
            continue;
        }

        prvEOL = printWithTabs(line, depth * prvEOL);
    }

    arena_free(&arena);
}

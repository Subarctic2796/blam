#include "ast.h"
#include "arena.h"
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
    case STMT_WHILE: ret->as.while_ = (StmtWhile){opts.cond, opts.bodyw}; break;
    case STMT_CLASS:
        ret->as.klass =
            (StmtClass){opts.fields, opts.methods, opts.scope, opts.superClass};
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
        for (size_t i = 0; i < klass.fields.cnt; i++) {
            Token name = klass.fields.items[i];
            printf(" %.*s", (int)name.cnt, name.items);
        }
        for (size_t i = 0; i < klass.methods.cnt; i++) {
            putchar(' ');
            printStmt(klass.methods.items[i]);
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

static const string RHS_INDENT = {NULL, ~0};
static const string LHS_INDENT = {NULL, ~1};

// name must end with a '('
#define prettyPrintStart(ap, name)                                             \
    do {                                                                       \
        arena_da_append(&(ap)->arena, &(ap)->lines, strLit(name));             \
        arena_da_append(&(ap)->arena, &(ap)->lines, RHS_INDENT);               \
    } while (0)

#define prettyPrintEnd(ap)                                                     \
    do {                                                                       \
        arena_da_append(&(ap)->arena, &(ap)->lines, LHS_INDENT);               \
        arena_da_append(&(ap)->arena, &(ap)->lines, strLit(")"));              \
    } while (0)

static void prettyPrintExpr(AstPrettyPrinter *ap, const Expr *expr);
static void prettyPrintStmt(AstPrettyPrinter *ap, const Stmt *stmt);

static void prettyPrintAddf(AstPrettyPrinter *ap, const char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    size_t n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    char *buf = (char *)arena_alloc(&ap->arena, n + 1);
    va_start(args, fmt);
    vsnprintf(buf, n + 1, fmt, args);
    va_end(args);

    arena_da_append(&ap->arena, &ap->lines, newStr(buf, n));
}

#define prettyPrintAdd(ap, s) arena_da_append(&ap->arena, &ap->lines, strLit(s))

static void prettyPrintExpr(AstPrettyPrinter *ap, const Expr *expr) {
    Token tok = expr->token;
    switch (expr->type) {
    case EXPR_THIS: prettyPrintAdd(ap, "This"); break;
    case EXPR_LITERAL:
        prettyPrintAddf(ap, "Literal(%.*s)", (int)tok.cnt, tok.items);
        break;
    case EXPR_SUPER:
        prettyPrintAddf(ap, "Super(%.*s)", (int)expr->as.method.cnt,
                        expr->as.method.items);
        break;
    case EXPR_IDENT: {
        ExprIdent ident = expr->as.ident;
        prettyPrintAddf(ap, "Ident(%.*s[%s:%d:%d])", (int)ident.name.cnt,
                        ident.name.items, ScopeTypeStr(ident.scope),
                        ident.depth, ident.index);
    } break;
    case EXPR_LOGICAL:
    case EXPR_BINARY:  {
        ExprBinary bin = expr->as.binary;
        prettyPrintStart(ap, "Binary(");

        prettyPrintAdd(ap, "left=");
        prettyPrintExpr(ap, bin.lhs);
        prettyPrintAddf(ap, "op='%.*s',", (int)tok.cnt, tok.items);
        prettyPrintAdd(ap, "right=");
        prettyPrintExpr(ap, bin.rhs);

        prettyPrintEnd(ap);
    } break;
    case EXPR_UNARY: {
        prettyPrintStart(ap, "Unary(");
        prettyPrintAddf(ap, "op='%.*s',", (int)tok.cnt, tok.items);
        prettyPrintAdd(ap, "operand=");
        prettyPrintExpr(ap, expr->as.right);
        prettyPrintEnd(ap);
    } break;
    case EXPR_ARRAY: {
        Exprs arr = expr->as.elements;
        if (arr.cnt == 0) {
            prettyPrintAdd(ap, "Array()");
            return;
        }
        prettyPrintStart(ap, "Array(");
        for (size_t i = 0; i < arr.cnt; i++) {
            prettyPrintExpr(ap, arr.items[i]);
        }
        prettyPrintEnd(ap);
    } break;
    case EXPR_HASH: {
        Exprs map = expr->as.elements;
        if (map.cnt == 0) {
            prettyPrintAdd(ap, "Map()");
            return;
        }
        prettyPrintStart(ap, "Map(");
        for (size_t i = 0; i < map.cnt; i += 2) {
            prettyPrintAdd(ap, "key=");
            prettyPrintExpr(ap, map.items[i]);
            prettyPrintAdd(ap, "value=");
            prettyPrintExpr(ap, map.items[i + 1]);
        }
        prettyPrintEnd(ap);
    } break;
    case EXPR_ASSIGN: {
        ExprAssign assign = expr->as.assign;

        prettyPrintStart(ap, "Assign(");
        prettyPrintAddf(ap, "op='%.*s',", (int)assign.oper.cnt,
                        assign.oper.items);
        prettyPrintAddf(ap, "ident='%.*s',", (int)tok.cnt, tok.items);
        prettyPrintAdd(ap, "value=");
        prettyPrintExpr(ap, assign.value);
        prettyPrintEnd(ap);
    } break;
    case EXPR_GROUPING: {
        prettyPrintStart(ap, "Group(");
        prettyPrintExpr(ap, expr->as.group);
        prettyPrintEnd(ap);
    } break;
    case EXPR_CALL: {
        ExprCall call = expr->as.call;
        prettyPrintStart(ap, "Call(");

        prettyPrintAdd(ap, "Callee=");
        prettyPrintExpr(ap, call.callee);

        if (call.args.cnt == 0) {
            prettyPrintAdd(ap, "args=()");
        } else {
            prettyPrintStart(ap, "args=(");
            for (size_t i = 0; i < call.args.cnt; i++) {
                prettyPrintExpr(ap, call.args.items[i]);
            }
            prettyPrintEnd(ap);
        }

        prettyPrintEnd(ap);
    } break;
    case EXPR_GET: {
        prettyPrintStart(ap, "Get(");

        prettyPrintAdd(ap, "object=");
        prettyPrintExpr(ap, expr->as.get);
        prettyPrintAdd(ap, ",");

        prettyPrintAddf(ap, "name='%.*s'", (int)tok.cnt, tok.items);

        prettyPrintEnd(ap);
    } break;
    case EXPR_SET: {
        ExprSet set = expr->as.set;
        prettyPrintStart(ap, "Set(");

        prettyPrintAdd(ap, "object=");
        prettyPrintExpr(ap, set.object);
        prettyPrintAdd(ap, ",");

        prettyPrintAddf(ap, "name='%.*s',", (int)tok.cnt, tok.items);
        prettyPrintAdd(ap, "value=");
        prettyPrintExpr(ap, set.value);

        prettyPrintEnd(ap);
    } break;
    case EXPR_INDEXED_GET: {
        ExprIndexedGet get = expr->as.indexedGet;
        prettyPrintStart(ap, "IndexedGet(");

        prettyPrintAdd(ap, "object=");
        prettyPrintExpr(ap, get.object);
        prettyPrintAdd(ap, ",");

        prettyPrintAdd(ap, "index=");
        prettyPrintExpr(ap, get.index);

        prettyPrintEnd(ap);
    } break;
    case EXPR_INDEXED_SET: {
        ExprIndexedSet set = expr->as.indexedSet;
        prettyPrintStart(ap, "IndexedSet(");

        prettyPrintAdd(ap, "object=");
        prettyPrintExpr(ap, set.object);
        prettyPrintAdd(ap, ",");

        prettyPrintAdd(ap, "index=");
        prettyPrintExpr(ap, set.index);
        prettyPrintAdd(ap, ",");

        prettyPrintAdd(ap, "value=");
        prettyPrintExpr(ap, set.value);

        prettyPrintEnd(ap);
    } break;
    case EXPR_IF: {
        StmtIf if_ = expr->as.if_->as.if_;
        prettyPrintStart(ap, "IfExpr(");

        prettyPrintAdd(ap, "Cond=");
        prettyPrintExpr(ap, if_.cond);
        prettyPrintAdd(ap, ",");

        prettyPrintAdd(ap, "Then=");
        prettyPrintStmt(ap, if_.then);

        if (if_.elze != NULL) {
            prettyPrintAdd(ap, ",");
            prettyPrintAdd(ap, "Else=");
            prettyPrintStmt(ap, if_.elze);
        }

        prettyPrintEnd(ap);
    } break;
    case EXPR_LAMBDA: {
        StmtFn fn = expr->as.lambda->as.fun;
        prettyPrintStart(ap, "Lambda(");

        if (fn.params.cnt == 0) {
            prettyPrintAdd(ap, "params=()");
        } else {
            prettyPrintStart(ap, "params=(");
            for (size_t i = 0; i < fn.params.cnt; i++) {
                Token param = fn.params.items[i];
                prettyPrintAddf(ap, "%.*s,", (int)param.cnt, param.items);
            }
            prettyPrintEnd(ap);
        }

        prettyPrintAdd(ap, "body=");
        for (size_t i = 0; i < fn.body.cnt; i++) {
            prettyPrintStmt(ap, fn.body.items[i]);
        }

        prettyPrintEnd(ap);
    } break;
    }
}

static void prettyPrintStmt(AstPrettyPrinter *ap, const Stmt *stmt) {
    Token tok = stmt->token;
    switch (stmt->type) {
    case STMT_EXPR: {
        prettyPrintStart(ap, "ExprStmt(");
        prettyPrintExpr(ap, stmt->as.expr);
        prettyPrintEnd(ap);
    } break;
    case STMT_VAR: {
        StmtVar var = stmt->as.var;
        prettyPrintStart(ap, "Var(");

        ExprIdent ident = var.name;
        prettyPrintAddf(ap, "name=\"%.*s\"[%s:%d:%d],", (int)tok.cnt, tok.items,
                        ScopeTypeStr(ident.scope), ident.depth, ident.index);

        if (var.init != NULL) {
            prettyPrintAdd(ap, "init=");
            prettyPrintExpr(ap, var.init);
        }

        prettyPrintEnd(ap);
    } break;
    case STMT_BLOCK: {
        Stmts block = stmt->as.block;
        if (block.cnt == 0) {
            prettyPrintAdd(ap, "Block()");
            return;
        }
        prettyPrintStart(ap, "Block(");
        for (size_t i = 0; i < block.cnt; i++) {
            prettyPrintStmt(ap, block.items[i]);
        }
        prettyPrintEnd(ap);
    } break;
    case STMT_CLASS: {
        StmtClass klass = stmt->as.klass;

        prettyPrintStart(ap, "Class(");

        if (klass.superClass.scope != SCOPE_NONE) {
            Token sc = klass.superClass.name;
            prettyPrintAddf(ap, "superclass=\"%.*s\",", (int)sc.cnt, sc.items);
        } else {
            prettyPrintAdd(ap, "superclass=\"\",");
        }
        prettyPrintAddf(ap, "name=\"%.*s\",", (int)tok.cnt, tok.items);

        if (klass.fields.cnt == 0) {
            prettyPrintAdd(ap, "fields=()");
        } else {
            prettyPrintStart(ap, "fields=(");
            for (size_t i = 0; i < klass.fields.cnt; i++) {
                Token field = klass.fields.items[i];
                prettyPrintAddf(ap, "%.*s,", (int)field.cnt, field.items);
            }
            prettyPrintEnd(ap);
        }

        if (klass.methods.cnt == 0) {
            prettyPrintAdd(ap, "methods=()");
        } else {
            prettyPrintStart(ap, "methods=(");
            for (size_t i = 0; i < klass.methods.cnt; i++) {
                prettyPrintStmt(ap, klass.methods.items[i]);
            }
            prettyPrintEnd(ap);
        }

        prettyPrintEnd(ap);
    } break;
    case STMT_CONTROL: {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        switch (tok.type) {
        case TOKEN_RETURN: {
            if (stmt->as.value != NULL) {
                prettyPrintStart(ap, "Return(");
                prettyPrintExpr(ap, stmt->as.value);
                prettyPrintEnd(ap);
            } else {
                prettyPrintAdd(ap, "Return()");
            }
        } break;
        case TOKEN_BREAK:    prettyPrintAdd(ap, "Break()"); break;
        case TOKEN_CONTINUE: prettyPrintAdd(ap, "Continue()"); break;
        default:
            UNREACHABLE("control type is not return, break, or continue");
            break;
        }
#pragma GCC diagnostic pop
    } break;
    case STMT_FUN: {
        StmtFn fn = stmt->as.fun;
        prettyPrintStart(ap, "Func(");

        prettyPrintAddf(ap, "name=\"%.*s\",", (int)tok.cnt, tok.items);

        if (fn.params.cnt == 0) {
            prettyPrintAdd(ap, "params=()");
        } else {
            prettyPrintStart(ap, "params=(");
            for (size_t i = 0; i < fn.params.cnt; i++) {
                Token param = fn.params.items[i];
                prettyPrintAddf(ap, "%.*s,", (int)param.cnt, param.items);
            }
            prettyPrintEnd(ap);
        }

        if (fn.body.cnt == 0) {
            prettyPrintAdd(ap, "body=()");
        } else {
            prettyPrintStart(ap, "body=(");
            for (size_t i = 0; i < fn.body.cnt; i++) {
                prettyPrintStmt(ap, fn.body.items[i]);
            }
            prettyPrintEnd(ap);
        }

        prettyPrintEnd(ap);
    } break;
    case STMT_PRINT: {
        prettyPrintStart(ap, "Print(");
        prettyPrintExpr(ap, stmt->as.print);
        prettyPrintEnd(ap);
    } break;
    case STMT_IF: {
        StmtIf if_ = stmt->as.if_;
        prettyPrintStart(ap, "If(");

        prettyPrintAdd(ap, "Cond=");
        prettyPrintExpr(ap, if_.cond);

        prettyPrintAdd(ap, "Then=");
        prettyPrintStmt(ap, if_.then);

        if (if_.elze != NULL) {
            prettyPrintAdd(ap, "Else=");
            prettyPrintStmt(ap, if_.elze);
        }
        prettyPrintEnd(ap);
    } break;
    case STMT_WHILE: {
        StmtWhile while_ = stmt->as.while_;
        prettyPrintStart(ap, "While(");

        prettyPrintAdd(ap, "Cond=");
        prettyPrintExpr(ap, while_.cond);

        prettyPrintAdd(ap, "Body=");
        prettyPrintStmt(ap, while_.body);

        prettyPrintEnd(ap);
    } break;
    case STMT_FOR_IN: {
        StmtForIn forin = stmt->as.forIn;
        prettyPrintStart(ap, "ForIn(");

        int cnt = 0;
        const char *items = NULL;
        if (forin.index.scope != SCOPE_NONE) {
            cnt = (int)forin.index.name.cnt;
            items = forin.index.name.items;
        }
        prettyPrintAddf(ap, "Index='%.*s',", cnt, items);

        Token name = forin.name.name;
        prettyPrintAddf(ap, "Name='%.*s',", (int)name.cnt, name.items);

        prettyPrintAdd(ap, "Iter=");
        prettyPrintExpr(ap, forin.iter);
        prettyPrintAdd(ap, ",");

        prettyPrintAdd(ap, "Body=");
        prettyPrintStmt(ap, forin.body);

        prettyPrintEnd(ap);
    } break;
    }
}

static inline bool printWithTabs(const string line, int depth) {
    if (line.cnt != 0) {
        switch (line.items[line.cnt - 1]) {
        case '(':
        case ')':
        case ',': {
            printf("%*s%.*s\n", depth * 4, "", (int)line.cnt, line.items);
            return true;
        }
        }
    }
    printf("%*s%.*s", depth * 4, "", (int)line.cnt, line.items);
    return false;
}

void astPrettyPrint(AstPrettyPrinter *ap, const Stmts prog) {
    ap->lines = (strings){0};
    arena_reset(&ap->arena);

    if (prog.cnt == 0) {
        puts("Program()");
        return;
    }

    prettyPrintStart(ap, "Program(");
    for (size_t i = 0; i < prog.cnt; i++) {
        prettyPrintStmt(ap, prog.items[i]);
    }
    prettyPrintEnd(ap);

    int depth = 0;
    bool prvEOL = true;
    for (size_t i = 0; i < ap->lines.cnt; i++) {
        string line = ap->lines.items[i];
        if (line.cnt == RHS_INDENT.cnt) {
            depth++;
            continue;
        } else if (line.cnt == LHS_INDENT.cnt) {
            depth--;
            continue;
        }

        prvEOL = printWithTabs(line, depth * prvEOL);
    }
}

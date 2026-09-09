#include <math.h>

#include "ast.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "opcode.h"
#include "token.h"
#include "value.h"
#include "vm.h"

typedef struct Loop {
    struct Loop *enclosing;
    int start, body, end;
    int scopeDepth;
} Loop;

typedef struct {
    OpCode exitOP;
    int depth;
} Local;

typedef struct {
    size_t cnt, cap;
    Local *items;
} Locals;

typedef struct Compiler {
    bool hadErr, isExpr, isAssign;
    FnType type;
    int scopeDepth;
    VM *vm;
    ObjFn *fn;
    Loop *loop;
    ValueMap constantsTable;
    Arena arena;
    Locals locals;
    Token token;
} Compiler;

static_assert(COMPILER_SIZE == sizeof(Compiler), "Size of compiler changed");

void initCompiler(Compiler *c) { *c = (Compiler){0}; }

void freeCompiler(Compiler *c) {
    arena_free(&c->arena);
    freeValueMap(c->vm, &c->constantsTable);
    TODO("");
}

static void resetCompiler(Compiler *c, VM *vm) {
    Arena arena = c->arena;
    Locals locals = c->locals;
    locals.cnt = 0;
    ValueMap constantsTable = c->constantsTable;

    *c = (Compiler){0};

    c->locals = locals;
    c->constantsTable = constantsTable;
    c->vm = vm;
    c->arena = arena;
    arena_reset(&c->arena);
    // TODO("");
}

// =======
// HELPERS
// =======

static void error(Compiler *c, const char *msg) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (c->token.type) {
    case TOKEN_EOF:
        fprintf(stderr, "[line %d] Error at end: %s\n", c->token.line, msg);
        break;
    case TOKEN_ERROR:
        fprintf(stderr, "[line %d] Error: %s\n", c->token.line, msg);
        break;
    default:
        fprintf(stderr, "[line %d] Error at '%.*s': %s\n", c->token.line,
                (int)c->token.cnt, c->token.items, msg);
        break;
    }
#pragma GCC diagnostic pop
    c->hadErr = true;
}

static inline Chunk *curChunk(const Compiler *c) { return &c->fn->chunk; }

static inline void emitByte(Compiler *c, uint8_t byte) {
    UNUSED(c);
    UNUSED(byte);
    TODO("");
}

static inline void emitBytes(Compiler *c, uint8_t byte1, uint8_t byte2) {
    emitByte(c, byte1);
    emitByte(c, byte2);
}

static inline void emitShort(Compiler *c, int arg) {
    emitBytes(c, (arg >> 8) & 0xff, arg & 0xff);
}

static inline void emitOp(Compiler *c, OpCode op) { emitByte(c, (uint8_t)op); }
static inline void emitPop(Compiler *c) { emitOp(c, OP_POP); }

static inline void emitOpArg(Compiler *c, OpCode op, uint8_t arg) {
    emitBytes(c, op, arg);
}

static inline void emitOp2Args(Compiler *c, OpCode op, int arg1, int arg2) {
    emitOp(c, op);
    emitBytes(c, arg1, arg2);
}

static void emitLoop(Compiler *c, int loopStart) {
    emitOp(c, OP_LOOP);

    int offset = curChunk(c)->cnt - loopStart + 2;
    if (offset > UINT16_MAX) error(c, "Loop body too large");

    emitShort(c, offset);
}

static inline int emitJump(Compiler *c, uint8_t inst) {
    emitOp2Args(c, inst, 0xff, 0xff);
    return curChunk(c)->cnt - 2;
}

static void patchJump(Compiler *c, int offset) {
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = curChunk(c)->cnt - offset - 2;

    if (jump > UINT16_MAX) error(c, "Too much code to jump over");

    curChunk(c)->code[offset] = (jump >> 8) & 0xff;
    curChunk(c)->code[offset + 1] = jump & 0xff;
}

static void emitReturn(Compiler *c) {
    // TODO: if c->type == method or function
    // and last opcode is OP_RETURN then don't emit
    // OP_NIL, and OP_RETURN
    if (c->type == FN_INIT) {
        emitOpArg(c, OP_GET_LOCAL, 0);
    } else {
        emitOp(c, OP_NIL);
    }
    emitOp(c, OP_RETURN);
}

static uint16_t makeConst(Compiler *c, Value value);

static inline void emitConstant(Compiler *c, Value value) {
    emitOpArg(c, OP_CONSTANT, makeConst(c, value));
}

static inline int getArgCount(const uint8_t *code, const ValueArray constants,
                              const int ip) {
    int argc = getArgCountForOp((OpCode)code[ip]);
    if (argc != -1) return argc;

    int constant = code[ip + 1];
    ObjFn *loadedFn = AS_FUNCTION(constants.items[constant]);

    // There is one byte for the constant, then two for each upvalue.
    return 1 + (loadedFn->upvalueCnt * 2);
}

static inline void initLoop(Compiler *c, Loop *loop) {
    *loop = (Loop){
        c->loop, curChunk(c)->cnt, 0, -1, c->scopeDepth,
    };
    c->loop = loop;
}

static void endLoop(Compiler *c) {
    emitLoop(c, c->loop->start);
    patchJump(c, c->loop->end);

    int i = c->loop->body;
    Chunk *chunk = curChunk(c);
    while (i < chunk->cnt) {
        if (chunk->code[i] == OP_NOP) {
            chunk->code[i] = OP_JUMP;
            patchJump(c, i + 1);
            i += 3;
        } else {
            i += 1 + getArgCount(chunk->code, chunk->constants, i);
        }
    }

    c->loop = c->loop->enclosing;
}

// discards any locals created
static inline void discardLocals(Compiler *c, int depth) {
    int i = c->locals.cnt - 1;
    while (i >= 0 && c->locals.items[i].depth > depth) {
        emitOp(c, c->locals.items[i].exitOP);
        i--;
    }
}

static inline void beginScope(Compiler *c) { c->scopeDepth++; }

static void endScope(Compiler *c) {
    c->scopeDepth--;
    while (c->locals.cnt > 0 &&
           c->locals.items[c->locals.cnt - 1].depth > c->scopeDepth) {
        Local *local = &c->locals.items[c->locals.cnt - 1];
        emitOp(c, local->exitOP);
        *local = (Local){0};
        c->locals.cnt--;
    }
}

static uint16_t makeConst(Compiler *c, Value value) {
    Value existing = EMPTY_VAL;
    if (valueMapGet(&c->constantsTable, value, &existing)) {
        return (uint16_t)AS_NUMBER(existing); // reuse existing constant
    }

    // add constant
    // make sure not collected
    if (IS_OBJ(value)) pushRoot(c->vm, value);
    int constIdx = addConst(c->vm, curChunk(c), value);
    // its safe so can remove it from temp roots
    if (IS_OBJ(value)) popRoot(c->vm);

    if (constIdx > UINT16_MAX) {
        error(c, "Too many constants in on chunk");
        return 0;
    }

    valueMapSet(c->vm, &c->constantsTable, value, NUMBER_VAL(constIdx));
    return (uint16_t)constIdx;
}

typedef enum {
    IDENT_IDENT,
    IDENT_CLASS_LOCAL,
    IDENT_VAR,
    IDENT_CLASS_GLOBAL,
} IdentType;

static uint16_t indentifierConst(Compiler *c, ExprIdent ident, IdentType type,
                                 uint16_t *classGlobal) {
    ObjString *identC = copyString(c->vm, ident.name.items, ident.name.cnt);
    switch (type) {
    case IDENT_IDENT:
    case IDENT_CLASS_LOCAL:  return makeConst(c, OBJ_VAL(identC));
    case IDENT_VAR:
    case IDENT_CLASS_GLOBAL: {
        // TODO: determine if is undefined global
        Value index = EMPTY_VAL;
        if (valueMapGet(&c->vm->globalNames, OBJ_VAL(identC), &index)) {
            return (uint16_t)AS_NUMBER(index);
        }

        pushRoot(c->vm, OBJ_VAL(identC));

        uint16_t newIndex = (uint16_t)c->vm->globalValues.cnt;
        writeValueArray(c->vm, &c->vm->globalValues, EMPTY_VAL);
        valueMapSet(c->vm, &c->vm->globalNames, OBJ_VAL(identC),
                    NUMBER_VAL(newIndex));

        popRoot(c->vm);

        if (type == IDENT_CLASS_GLOBAL) {
            *classGlobal = newIndex;
            return makeConst(c, OBJ_VAL(identC));
        }
        return newIndex;
    }
    default: return 0;
    }
}

static inline uint16_t syntheticIdentifierConst(Compiler *c, stringView name,
                                                bool isVar) {
    Token tok = syntheticToken(name);
    ExprIdent ident = {SCOPE_NONE, false, -1, -1, tok};
    return indentifierConst(c, ident, isVar ? IDENT_VAR : IDENT_IDENT, NULL);
}

static void declareVar(Compiler *c, Token tok) {
    UNUSED(c);
    UNUSED(tok);
    TODO("");
}

static inline void markInit(Compiler *c) {
    UNUSED(c);
    TODO("");
}

static int addLocal(Compiler *c, ExprIdent name) {
    arena_da_append(&c->arena, &c->locals, ((Local){name.isLocal, name.depth}));
    return c->locals.cnt - 1;
}

// =========
// COMPILING
// =========

static ObjFn *endCompiler(Compiler *c, ObjFn *previous) {
    emitReturn(c);
    ObjFn *fn = c->fn;

#ifdef DEBUG_PRINT_CODE
    if (!c->hadErr) {
        disassembleChunk(c->vm, curChunk(c),
                         fn->name != NULL ? fn->name->items : "<script>");
    }
#endif /* ifdef DEBUG_PRINT_CODE */

    freeValueMap(c->vm, &c->constantsTable);
    c->fn = previous;
    return fn;
}

static inline void compileLiteral(Compiler *c, Token lit) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (lit.type) {
    case TOKEN_FALSE: emitOp(c, OP_FALSE); break;
    case TOKEN_TRUE:  emitOp(c, OP_TRUE); break;
    case TOKEN_NIL:   emitOp(c, OP_NIL); break;
    case TOKEN_STRING:
        emitConstant(c, OBJ_VAL(copyString(c->vm, lit.items + 1, lit.cnt - 2)));
        break;
    case TOKEN_NUMBER: {
        double value = strtod(lit.items, NULL);
        if (trunc(value) == value) {
            if (value <= UINT8_MAX) {
                emitOpArg(c, OP_SMALL_INT, (uint8_t)(uint64_t)value);
                return;
            }
        }
        emitConstant(c, NUMBER_VAL(value));
    } break;
    default:
        UNREACHABLE("literal type is not number, string, true, false, or nil");
        break;
    }
#pragma GCC diagnostic pop
}

static void compileIdent(Compiler *c, ExprIdent ident, bool isAssign) {
    OpCode op = OP_GET_GLOBAL;
    switch (ident.scope) {
    case SCOPE_LOCAL:   op = isAssign ? OP_SET_LOCAL : OP_GET_LOCAL; break;
    case SCOPE_UPVALUE: op = isAssign ? OP_SET_UPVALUE : OP_GET_UPVALUE; break;
    case SCOPE_GLOBAL:  {
        // TODO: resolve index see if global exists
        op = isAssign ? OP_SET_GLOBAL : OP_GET_GLOBAL;
        ident.index = indentifierConst(c, ident, IDENT_VAR, NULL);
    } break;
    case SCOPE_NONE:
    case __SCOPE_CNT:
    default:          UNREACHABLE("unknown scope type"); break;
    }

    emitOpArg(c, op, ident.index);
}

static void compileExpr(Compiler *c, const Expr *expr);
static void compileStmt(Compiler *c, const Stmt *stmt);

static void compileExpr(Compiler *c, const Expr *expr) {
    Token token = expr->token;
    c->token = token;

    switch (expr->type) {
    case EXPR_GROUPING: compileExpr(c, expr->as.group); break;
    case EXPR_LITERAL:  compileLiteral(c, token); break;
    case EXPR_IDENT:    compileIdent(c, expr->as.ident, c->isAssign); break;
    case EXPR_ARRAY:    {
        Exprs arr = expr->as.elements;
        for (size_t i = 0; i < arr.cnt; i++) {
            compileExpr(c, arr.items[i]);
        }
        emitOpArg(c, OP_BUILD_ARRAY, arr.cnt);
    } break;
    case EXPR_HASH: {
        Exprs map = expr->as.elements;
        for (size_t i = 0; i < map.cnt; i += 2) {
            compileExpr(c, map.items[i]);
            compileExpr(c, map.items[i + 1]);
        }
        emitOpArg(c, OP_BUILD_ARRAY, map.cnt / 2);
    } break;
    case EXPR_LAMBDA: {
        c->isExpr = true;
        compileStmt(c, expr->as.lambda);
        c->isExpr = false;
    } break;
    case EXPR_IF: {
        c->isExpr = true;
        compileStmt(c, expr->as.if_);
        c->isExpr = false;
    } break;
    case EXPR_BINARY: {
        ExprBinary bin = expr->as.binary;
        compileExpr(c, bin.lhs);
        compileExpr(c, bin.rhs);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        switch (token.type) {
        case TOKEN_NEQ:     emitOp(c, OP_NOT_EQUAL); break;
        case TOKEN_EQEQ:    emitOp(c, OP_EQUAL); break;
        case TOKEN_GT:      emitOp(c, OP_GREATER); break;
        case TOKEN_GTEQ:    emitOp(c, OP_GREATER_EQUAL); break;
        case TOKEN_LT:      emitOp(c, OP_LESS); break;
        case TOKEN_LTEQ:    emitOp(c, OP_LESS_EQUAL); break;
        case TOKEN_PLUS:    emitOp(c, OP_ADD); break;
        case TOKEN_MINUS:   emitOp(c, OP_SUBTRACT); break;
        case TOKEN_STAR:    emitOp(c, OP_MULTIPLY); break;
        case TOKEN_SLASH:   emitOp(c, OP_DIVIDE); break;
        case TOKEN_PERCENT: emitOp(c, OP_MOD); break;
        case TOKEN_IN:
            error(c,
                  "compiling an '`item` in `collection`' not implemented yet");
            break;
        default: return; // Unreachable.
        }
#pragma GCC diagnostic pop
    } break;
    case EXPR_LOGICAL: {
        ExprBinary bin = expr->as.binary;
        compileExpr(c, bin.lhs);

        if (token.type == TOKEN_AND) {
            int endJump = emitJump(c, OP_JUMP_IF_FALSE);
            emitPop(c);

            compileExpr(c, bin.rhs);

            patchJump(c, endJump);
        } else if (token.type == TOKEN_OR) {
            int elseJump = emitJump(c, OP_JUMP_IF_FALSE);
            int endJump = emitJump(c, OP_JUMP);

            patchJump(c, elseJump);
            emitPop(c);

            compileExpr(c, bin.rhs);

            patchJump(c, endJump);
        } else {
            UNREACHABLE("logical operator is not 'and' or 'or'");
        }
    } break;
    case EXPR_UNARY: {
        compileExpr(c, expr->as.right);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
        // emit the operator instruction
        switch (token.type) {
        case TOKEN_BANG:  emitOp(c, OP_NOT); break;
        case TOKEN_MINUS: emitOp(c, OP_NEGATE); break;
        default:          return;
        }
#pragma GCC diagnostic pop
    } break;
    case EXPR_ASSIGN: {
        ExprAssign assign = expr->as.assign;
        c->isAssign = true;
        compileExpr(c, assign.value);
        c->isAssign = false;
    } break;
    case EXPR_CALL: {
        ExprCall call = expr->as.call;
        compileExpr(c, call.callee);
        for (size_t i = 0; i < call.args.cnt; i++) {
            compileExpr(c, call.args.items[i]);
        }
        emitOpArg(c, OP_CALL, (uint8_t)call.args.cnt);
    } break;
    case EXPR_GET:
    case EXPR_INDEXED_GET:
    case EXPR_INDEXED_SET:
    case EXPR_SET:
    case EXPR_SUPER:
    case EXPR_THIS:        TODO(""); break;
    }
}

static inline void compileControlStmt(Compiler *c, const Stmt *stmt) {
    c->token = stmt->token;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
    switch (stmt->token.type) {
    case TOKEN_RETURN: {
        if (stmt->as.value == NULL) {
            emitReturn(c);
        } else {
            compileExpr(c, stmt->as.value);
            emitOp(c, OP_RETURN);
        }
    } break;
    case TOKEN_BREAK: {
        discardLocals(c, c->loop->scopeDepth + 1);
        emitJump(c, OP_NOP);
    } break;
    case TOKEN_CONTINUE: {
        discardLocals(c, c->loop->scopeDepth + 1);
        emitLoop(c, c->loop->start);
    } break;
    default:
        UNREACHABLE("control type is not return, break, or continue");
        break;
    }
#pragma GCC diagnostic pop
}

static void compileStmt(Compiler *c, const Stmt *stmt) {
    Token token = stmt->token;
    c->token = token;
    switch (stmt->type) {
    case STMT_CONTROL: compileControlStmt(c, stmt); break;
    case STMT_EXPR:    {
        compileExpr(c, stmt->as.expr);
        if (!c->isExpr) emitPop(c);
    } break;
    case STMT_PRINT: {
        compileExpr(c, stmt->as.print);
        emitOp(c, OP_PRINT);
    } break;
    case STMT_BLOCK: {
        beginScope(c);
        for (size_t i = 0; i < stmt->as.block.cnt; i++) {
            compileStmt(c, stmt->as.block.items[i]);
        }
        endScope(c);
    } break;
    case STMT_WHILE: {
        Loop loop = {0};
        initLoop(c, &loop);

        StmtWhile while_ = stmt->as.while_;

        compileExpr(c, while_.cond);

        loop.end = emitJump(c, OP_JUMP_IF_FALSE);
        emitPop(c);

        loop.body = curChunk(c)->cnt;
        compileStmt(c, while_.body);
        endLoop(c);
    } break;
    case STMT_IF: {
        StmtIf if_ = stmt->as.if_;
        compileExpr(c, if_.cond);

        int thenJump = emitJump(c, OP_JUMP_IF_FALSE);
        emitPop(c); // true branch pop
        compileStmt(c, if_.then);

        int elseJump = emitJump(c, OP_JUMP);
        patchJump(c, thenJump);
        emitPop(c); // false branch pop

        if (if_.elze != NULL) compileStmt(c, if_.elze);
        patchJump(c, elseJump);
    } break;
    case STMT_CLASS: {
        // StmtClass klass = stmt->as.klass;
        TODO("STMT_CLASS");
    } break;
    case STMT_FUN: {
        // TODO: this is not safe from the GC make it safe
        ObjFn *previous = c->fn;

        // TODO: new ObjFn
        StmtFn fn = stmt->as.fun;
        c->fn->upvalueCnt = fn.upvaluesCnt;

        beginScope(c);
        for (size_t i = 0; i < fn.params.cnt; i++) {
            declareVar(c, fn.params.items[i]);
            markInit(c);
        }

        for (size_t i = 0; i < fn.body.cnt; i++) {
            compileStmt(c, fn.body.items[i]);
        }

        ObjFn *func = endCompiler(c, previous);
        endScope(c);

        emitOpArg(c, OP_CLOSURE, makeConst(c, OBJ_VAL(func)));

        for (int i = 0; i < fn.upvaluesCnt; i++) {
            // TODO: actually do upvalue stuff
            bool isLocal = false;
            uint8_t index = 0;
            emitBytes(c, isLocal, index);
        }
        TODO("STMT_FUN");
    } break;
    case STMT_VAR: {
        StmtVar var = stmt->as.var;
        if (var.init != NULL) {
            c->isAssign = true;
            compileExpr(c, var.init);
        } else {
            emitOp(c, OP_NIL);
        }

        if (var.name.scope == SCOPE_GLOBAL) {
            if (c->scopeDepth > 0) addLocal(c, var.name);
            compileIdent(c, var.name, c->isAssign);
        } else {
            markInit(c);
        }

        if (var.init != NULL) c->isAssign = false;
        TODO("STMT_VAR");
    } break;
    case STMT_FOR_IN: {
        StmtForIn forIn = stmt->as.forIn;

        beginScope(c);

        emitOpArg(c, OP_GET_GLOBAL,
                  syntheticIdentifierConst(c, svLit("Iter"), true));
        compileExpr(c, forIn.iter);
        emitOpArg(c, OP_CALL, 1);

        Token iter = syntheticToken(svLit("it "));
        int itSlot = addLocal(c, (ExprIdent){SCOPE_LOCAL, -1, -1, false, iter});
        markInit(c);

        int ixSlot = -1;
        if (forIn.index.scope != SCOPE_NONE) {
            ixSlot = addLocal(c, forIn.index);
            markInit(c);
            emitOp(c, OP_NIL);
        }

        int iSlot = addLocal(c, forIn.name);
        markInit(c);
        emitOp(c, OP_NIL);

        Loop loop = {0};
        initLoop(c, &loop);

        // advance iterator
        emitOpArg(c, OP_GET_LOCAL, itSlot);
        emitOp2Args(c, OP_INVOKE,
                    syntheticIdentifierConst(c, svLit("next"), false), 0);

        // test the condition
        loop.end = emitJump(c, OP_JUMP_IF_FALSE);
        emitPop(c);

        // update ix
        if (forIn.index.scope != SCOPE_NONE) {
            emitOpArg(c, OP_GET_LOCAL, itSlot);
            emitOp2Args(c, OP_INVOKE,
                        syntheticIdentifierConst(c, svLit("index"), false), 0);
            emitOpArg(c, OP_SET_LOCAL, ixSlot);
            emitPop(c);
        }

        // update i
        emitOpArg(c, OP_GET_LOCAL, itSlot);
        emitOp2Args(c, OP_INVOKE,
                    syntheticIdentifierConst(c, svLit("value"), false), 0);
        emitOpArg(c, OP_SET_LOCAL, iSlot);
        emitPop(c);

        // compile the body
        loop.body = curChunk(c)->cnt;
        compileStmt(c, forIn.body);

        endLoop(c);
        endScope(c);
        TODO("STMT_FOR_IN");
    } break;
    }
}

ObjFn *compile(VM *vm, Compiler *c, const Stmts stmts) {
    resetCompiler(c, vm);

    for (size_t i = 0; i < stmts.cnt; i++) {
        compileStmt(c, stmts.items[i]);
    }

    return c->hadErr ? NULL : c->fn;
}

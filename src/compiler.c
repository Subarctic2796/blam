#include "compiler.h"
#include "common.h"

typedef struct Compiler {
    void *stub;
} Compiler;

static_assert(COMPILER_SIZE == sizeof(Compiler), "Size of compiler changed");

void initCompiler(Compiler *c) { *c = (Compiler){0}; }

void freeCompiler(Compiler *c) {
    UNUSED(c);
    TODO("");
}

void resetCompiler(Compiler *c) {
    UNUSED(c);
    TODO("");
}

ObjFn *compile(Compiler *c, Stmts stmts) {
    UNUSED(c);
    UNUSED(stmts);
    TODO("");
    return NULL;
}

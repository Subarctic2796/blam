#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "parser.h"

void initVM(VM *vm, Parser *parser, Compiler *compiler) {
    *vm = (VM){0};

    initParser(parser);
    initCompiler(compiler);

    vm->parser = parser;
    vm->compiler = compiler;
}

void freeVM(VM *vm) {
    freeParser(vm->parser);
    // freeCompiler(vm->compiler);
    TODO("");
}

InterpretResult interpret(VM *vm, const char *src) {
    static Stmts stmts = {0};
    if (!parse(vm, vm->parser, src, &stmts)) return INTERPRET_COMPILE_ERR;

    for (size_t i = 0; i < stmts.cnt; i++) {
        printStmt(stmts.items[i]);
        puts("");
    }

    return INTERPRET_OK;
}

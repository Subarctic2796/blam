#include "vm.h"
#include "common.h"
#include "parser.h"
#include "resolver.h"

// void initVM(VM *vm, Parser *parser, Resolver *resolver, Compiler *compiler) {
void initVM(VM *vm, Parser *parser, Resolver *resolver, void *compiler) {
    initParser(parser);
    initResolver(resolver);
    // initCompiler(compiler);
    *vm = (VM){parser, resolver, compiler};
}

void freeVM(VM *vm) {
    freeParser(vm->parser);
    freeResolver(vm->resolver);
    TODO("");
}

InterpretResult interpret(VM *vm, const char *src, Stmts *stmts) {
    resetParser(vm->parser, src);
    if (!parse(vm->parser, stmts)) return INTERPRET_COMPILE_ERR;

    resetResolver(vm->resolver);
    if (!resolve(vm->resolver, stmts)) return INTERPRET_COMPILE_ERR;
    for (size_t i = 0; i < stmts->cnt; i++) {
        printStmt(stmts->items[i]);
        puts("");
    }

    return INTERPRET_OK;
}

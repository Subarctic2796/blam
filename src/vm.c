#include "vm.h"
#include "ast.h"
#include "common.h"
#include "compiler.h"
#include "parser.h"
#include "value.h"

void initVM(VM *vm, Parser *parser, Compiler *compiler) {
    *vm = (VM){0};

    initParser(parser);
    initCompiler(compiler);

    vm->parser = parser;
    vm->compiler = compiler;
}

void freeVM(VM *vm) {
    freeParser(vm->parser);
    freeCompiler(vm->compiler);
    TODO("");
}

static AstPrinter AST_PRINTER = {0};

InterpretResult interpret(VM *vm, const char *src) {
    if (!parse(vm->parser, src)) return INTERPRET_COMPILE_ERR;

    Stmts stmts = parserGetStmts(vm->parser);
    initAstPrinter(&AST_PRINTER, stmts);
    astPrinterPrint(&AST_PRINTER);

    ObjFn *func = compile(vm, vm->compiler, stmts);
    if (func == NULL) return INTERPRET_COMPILE_ERR;

    return INTERPRET_OK;
}

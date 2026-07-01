#ifndef INCLUDE_SRC_VM_H_
#define INCLUDE_SRC_VM_H_

#include "parser.h"
#include "resolver.h"

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERR,
    INTERPRET_RUNTIME_ERR,
} InterpretResult;

typedef struct {
    Parser *parser;
    Resolver *resolver;
    // Compiler *compiler;
    void *compiler;
} VM;

// void initVM(VM *vm, Parser *parser, Resolver *resolver, Compiler *compiler);
void initVM(VM *vm, Parser *parser, Resolver *resolver, void *compiler);
void freeVM(VM *vm);
InterpretResult interpret(VM *vm, const char *src, Stmts *stmts);

#endif // INCLUDE_SRC_VM_H_

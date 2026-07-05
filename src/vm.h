#ifndef INCLUDE_SRC_VM_H_
#define INCLUDE_SRC_VM_H_

#include "compiler.h"
#include "parser.h"
#include "value.h"

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERR,
    INTERPRET_RUNTIME_ERR,
} InterpretResult;

typedef struct VM {
    Parser *parser;
    // Compiler *compiler;
    void *compiler;

    Obj *objects;

    ValueMap strings;
    ValueMap globalNames;
    ValueArray globalValues;
} VM;

void initVM(VM *vm, Parser *parser, Compiler *compiler);
void freeVM(VM *vm);
InterpretResult interpret(VM *vm, const char *src);

#endif // INCLUDE_SRC_VM_H_

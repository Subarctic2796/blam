#ifndef INCLUDE_SRC_VM_H_
#define INCLUDE_SRC_VM_H_

#include "compiler.h"
#include "parser.h"
#include "value.h"

#ifdef BLAM_DEBUG
#define VM_UNREACHABLE()                                                       \
    do {                                                                       \
        fprintf(stderr, "[%s:%d] in %s() should be unreachable\n", __FILE__,   \
                __LINE__, __func__);                                           \
        abort();                                                               \
    } while (0)
#else
#define VM_UNREACHABLE() __builtin_unreachable()
#endif // BLAM_DEBUG

#define MAX_TEMP_ROOTS 8

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERR,
    INTERPRET_RUNTIME_ERR,
} InterpretResult;

typedef struct VM {
    Parser *parser;
    Compiler *compiler;

    Obj *objects;

    ValueMap strings;
    ValueMap globalNames;
    ValueArray globalValues;

    int tempCnt;
    Value tempRoots[MAX_TEMP_ROOTS];
} VM;

void initVM(VM *vm, Parser *parser, Compiler *compiler);
void freeVM(VM *vm);
InterpretResult interpret(VM *vm, const char *src);

static inline void pushRoot(VM *vm, Value value) {
    vm->tempRoots[vm->tempCnt++] = value;
}

static inline void popRoot(VM *vm) { vm->tempCnt--; }

#endif // INCLUDE_SRC_VM_H_

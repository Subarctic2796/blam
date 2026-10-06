#ifndef INCLUDE_SRC_VM_H_
#define INCLUDE_SRC_VM_H_

#include "compiler.h"
#include "parser.h"
#include "value.h"

#define VM_ALLOC(type, cnt)                                                    \
    (type *)vmReallocate(vm, NULL, 0, sizeof(type) * cnt)

#define VM_FREE(type, ptr) vmReallocate(vm, ptr, sizeof(type), 0)

#define VM_GROW_ARRAY(type, ptr, oldCnt, newCnt)                               \
    (type *)vmReallocate(vm, ptr, sizeof(type) * (oldCnt),                     \
                         sizeof(type) * (newCnt))

#define VM_FREE_ARRAY(type, ptr, oldCnt)                                       \
    vmReallocate(vm, (ptr), sizeof(type) * (oldCnt), 0)

#define VM_da_append(type, da, item)                                           \
    do {                                                                       \
        if ((da)->cap < (da)->cnt + 1) {                                       \
            size_t oldcap = (da)->cap;                                         \
            (da)->cap = GROW_CAP(oldcap);                                      \
            (da)->items = VM_GROW_ARRAY(type, (da)->items, oldcap, (da)->cap); \
        }                                                                      \
        (da)->items[(da)->cnt++] = (item);                                     \
    } while (0)

#define MAX_TEMP_ROOTS 8

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERR,
    INTERPRET_RUNTIME_ERR,
} InterpretResult;

typedef struct VM {
    Parser *parser;
    Compiler *compiler;

    size_t bytesAllocated;
    size_t nextGC;
    Obj *objects;

    int grayCnt, grayCap;
    Obj **grayStack;

    ValueMap strings;
    ValueMap globalNames;
    ValueArray globalValues;

    int tempCnt;
    Value tempRoots[MAX_TEMP_ROOTS];
} VM;

void initVM(VM *vm, Parser *parser, Compiler *compiler);
void freeVM(VM *vm);
InterpretResult interpret(VM *vm, const char *src);
void *vmReallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize);

static inline void pushRoot(VM *vm, Value value) {
    vm->tempRoots[vm->tempCnt++] = value;
}

static inline void popRoot(VM *vm) { vm->tempCnt--; }

#endif // INCLUDE_SRC_VM_H_

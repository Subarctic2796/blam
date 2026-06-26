#ifndef INCLUDE_SRC_VM_H_
#define INCLUDE_SRC_VM_H_

typedef struct {
    // Parser *parser;
    // Compiler *compiler;
    void *parser;
    void *compiler;
} VM;

// void initVM(VM *vm, Parser *parser, Compiler *compiler) {
void initVM(VM *vm, void *parser, void *compiler);
void freeVM(VM *vm);

#endif // INCLUDE_SRC_VM_H_

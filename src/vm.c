#include "vm.h"
#include "common.h"

// void initVM(VM *vm, Parser *parser, Compiler *compiler) {
void initVM(VM *vm, void *parser, void *compiler) {
    *vm = (VM){parser, compiler};
}

void freeVM(VM *vm) {
    UNUSED(vm);
    TODO("");
}

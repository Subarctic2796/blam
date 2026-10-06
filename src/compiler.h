#ifndef INCLUDE_SRC_COMPILER_H_
#define INCLUDE_SRC_COMPILER_H_

#include "ast.h"
#include "value.h"

typedef struct VM VM;

#define COMPILER_SIZE 128
typedef struct Compiler Compiler;

void initCompiler(Compiler *c);
void freeCompiler(Compiler *c);
ObjFn *compile(VM *vm, Compiler *c, const Stmts stmts);
void markCompiler(VM *vm, Compiler *c);

#endif // INCLUDE_SRC_COMPILER_H_

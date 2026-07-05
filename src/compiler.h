#ifndef INCLUDE_SRC_COMPILER_H_
#define INCLUDE_SRC_COMPILER_H_

#include "ast.h"
#include "value.h"

#define COMPILER_SIZE 8
typedef struct Compiler Compiler;

void initCompiler(Compiler *c);
void freeCompiler(Compiler *c);
void resetCompiler(Compiler *c);
ObjFn *compile(Compiler *c, Stmts stmts);

#endif // INCLUDE_SRC_COMPILER_H_

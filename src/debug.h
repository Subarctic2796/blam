#ifndef INCLUDE_SRC_DEBUG_H_
#define INCLUDE_SRC_DEBUG_H_

#include "value.h"

int disassembleInst(const VM *vm, const Chunk *chunk, int offset);
void disassembleChunk(const VM *vm, const Chunk *chunk, const char *name);

#endif // INCLUDE_SRC_DEBUG_H_

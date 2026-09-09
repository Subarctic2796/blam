#include "debug.h"
#include "common.h"
#include "opcode.h"
#include "value.h"

void disassembleChunk(const VM *vm, const Chunk *chunk, const char *name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->cnt;) {
        offset = disassembleInst(vm, chunk, offset);
    }
}

static inline int simpleInst(OpCode op, int offset) {
    printf("%s\n", OpcodeStr(op));
    return offset + 1;
}

static inline int byteInst(OpCode op, const Chunk *chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", OpcodeStr(op), slot);
    return offset + 2;
}

static inline int jumpInst(OpCode op, int sign, const Chunk *chunk,
                           int offset) {
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", OpcodeStr(op), offset,
           offset + 3 + sign * jump);
    return offset + 3;
}

static inline int constantInst(OpCode op, const Chunk *chunk, int offset) {
    uint8_t constIdx = chunk->code[offset + 1];
    printf("%-16s %4d '", OpcodeStr(op), constIdx);
    printValue(chunk->constants.items[constIdx]);
    printf("'\n");
    return offset + 2;
}

static inline int invokeInst(OpCode op, const Chunk *chunk, int offset) {
    uint8_t idx = chunk->code[offset + 1];
    uint8_t argc = chunk->code[offset + 2];
    printf("%-16s (%d args) %4d '", OpcodeStr(op), argc, idx);
    printValue(chunk->constants.items[idx]);
    printf("'\n");
    return offset + 3;
}

int disassembleInst(const VM *vm, const Chunk *chunk, int offset) {
    UNUSED(vm);
    printf("%04d ", offset);
    int line = getLine(chunk, offset);
    if (offset > 0 && line == getLine(chunk, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", line);
    }

    OpCode op = (OpCode)chunk->code[offset];
    switch (op) {
    case OP_CLOSE_UPVALUE: return simpleInst(op, offset);
    case OP_RETURN:        return simpleInst(op, offset);
    case OP_INHERIT:       return simpleInst(op, offset);
    case OP_NOP:           return simpleInst(op, offset);
    case OP_NIL:           return simpleInst(op, offset);
    case OP_FALSE:         return simpleInst(op, offset);
    case OP_GET_INDEX:     return simpleInst(op, offset);
    case OP_SET_INDEX:     return simpleInst(op, offset);
    case OP_EQUAL:         return simpleInst(op, offset);
    case OP_NOT_EQUAL:     return simpleInst(op, offset);
    case OP_GREATER:       return simpleInst(op, offset);
    case OP_GREATER_EQUAL: return simpleInst(op, offset);
    case OP_LESS:          return simpleInst(op, offset);
    case OP_LESS_EQUAL:    return simpleInst(op, offset);
    case OP_TRUE:          return simpleInst(op, offset);
    case OP_ADD:           return simpleInst(op, offset);
    case OP_MOD:           return simpleInst(op, offset);
    case OP_SUBTRACT:      return simpleInst(op, offset);
    case OP_MULTIPLY:      return simpleInst(op, offset);
    case OP_DIVIDE:        return simpleInst(op, offset);
    case OP_NOT:           return simpleInst(op, offset);
    case OP_NEGATE:        return simpleInst(op, offset);
    case OP_PRINT:         return simpleInst(op, offset);
    case OP_POP:           return simpleInst(op, offset);
    case OP_CALL:          return byteInst(op, chunk, offset);
    case OP_SMALL_INT:     return byteInst(op, chunk, offset);
    case OP_GET_LOCAL:     return byteInst(op, chunk, offset);
    case OP_SET_LOCAL:     return byteInst(op, chunk, offset);
    case OP_GET_GLOBAL:    return byteInst(op, chunk, offset);
    case OP_DEFINE_GLOBAL: return byteInst(op, chunk, offset);
    case OP_SET_GLOBAL:    return byteInst(op, chunk, offset);
    case OP_GET_UPVALUE:   return byteInst(op, chunk, offset);
    case OP_SET_UPVALUE:   return byteInst(op, chunk, offset);
    case OP_BUILD_ARRAY:   return byteInst(op, chunk, offset);
    case OP_BUILD_MAP:     return byteInst(op, chunk, offset);
    case OP_CONSTANT:      return constantInst(op, chunk, offset);
    case OP_GET_PROPERTY:  return constantInst(op, chunk, offset);
    case OP_SET_PROPERTY:  return constantInst(op, chunk, offset);
    case OP_GET_SUPER:     return constantInst(op, chunk, offset);
    case OP_METHOD:        return constantInst(op, chunk, offset);
    case OP_CLASS:         return constantInst(op, chunk, offset);
    case OP_JUMP:          return jumpInst(op, 1, chunk, offset);
    case OP_JUMP_IF_FALSE: return jumpInst(op, 1, chunk, offset);
    case OP_LOOP:          return jumpInst(op, -1, chunk, offset);
    case OP_INVOKE:        return invokeInst(op, chunk, offset);
    case OP_SUPER_INVOKE:  return invokeInst(op, chunk, offset);
    case OP_CLOSURE:       {
        offset++;
        uint8_t idx = chunk->code[offset++];
        printf("%-16s %4d ", OpcodeStr(op), idx);
        printValue(chunk->constants.items[idx]);
        printf("\n");

        ObjFn *function = AS_FUNCTION(chunk->constants.items[idx]);
        for (int j = 0; j < function->upvalueCnt; j++) {
            int isLocal = chunk->code[offset++];
            int index = chunk->code[offset++];
            printf("%04d      |                     %s %d\n", offset - 2,
                   isLocal ? "local" : "upvalue", index);
        }
        return offset;
    }
    default: printf("Unknown opcode %d\n", op); return offset + 1;
    }
}

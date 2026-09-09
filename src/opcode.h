#ifndef INCLUDE_SRC_OPCODE_H_
#define INCLUDE_SRC_OPCODE_H_

#include "common.h"

typedef enum {
#define OPCODE(name) OP_##name,
#include "opcodeCodes.h"
#undef OPCODE
} OpCode;

static inline const char *OpcodeStr(const OpCode op) {
    static const char *strings[] = {
#define OPCODE(name) "OP_##name",
#include "opcodeCodes.h"
#undef OPCODE
    };
    static_assert(ARRAY_LEN(strings) == OP_CLOSURE + 1,
                  "number of Opcodes changed");
    return strings[op];
}

// returns -1 for OP_CLOSURE
// returns -2 on unknown opcode
static inline int getArgCountForOp(const OpCode op) {
    switch (op) {
    case OP_NOP:
    case OP_NIL:
    case OP_TRUE:
    case OP_FALSE:
    case OP_POP:
    case OP_EQUAL:
    case OP_NOT_EQUAL:
    case OP_GREATER:
    case OP_GREATER_EQUAL:
    case OP_LESS:
    case OP_LESS_EQUAL:
    case OP_ADD:
    case OP_MOD:
    case OP_SUBTRACT:
    case OP_MULTIPLY:
    case OP_DIVIDE:
    case OP_NOT:
    case OP_NEGATE:
    case OP_CLOSE_UPVALUE:
    case OP_RETURN:
    case OP_PRINT:
    case OP_INHERIT:
    case OP_GET_INDEX:
    case OP_SET_INDEX:     return 0;

    case OP_SMALL_INT:
    case OP_CONSTANT:
    case OP_GET_PROPERTY:
    case OP_SET_PROPERTY:
    case OP_GET_LOCAL:
    case OP_SET_LOCAL:
    case OP_GET_GLOBAL:
    case OP_SET_GLOBAL:
    case OP_DEFINE_GLOBAL:
    case OP_GET_UPVALUE:
    case OP_SET_UPVALUE:
    case OP_GET_SUPER:
    case OP_METHOD:
    case OP_BUILD_ARRAY:
    case OP_BUILD_MAP:     return 1;

    case OP_JUMP:
    case OP_JUMP_IF_FALSE:
    case OP_LOOP:
    case OP_CLASS:
    case OP_CALL:
    case OP_INVOKE:
    case OP_SUPER_INVOKE:  return 2;

    case OP_CLOSURE: return -1;
    }

    UNREACHABLE("unknown opcode");
    return -2;
}

#endif // INCLUDE_SRC_OPCODE_H_

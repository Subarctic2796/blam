#include "value.h"
#include "common.h"
#include "vm.h"

int getLine(const Chunk *chunk, int offset) {
    UNUSED(chunk);
    UNUSED(offset);
    TODO("");
    return 0;
}

void printValue(Value value) {
    UNUSED(value);
    TODO("");
}

ObjString *copyString(VM *vm, const char *chars, int length) {
    UNUSED(vm);
    UNUSED(chars);
    UNUSED(length);
    TODO("");
    return NULL;
}

void freeValueMap(VM *vm, ValueMap *map) {
    UNUSED(vm);
    UNUSED(map);
    TODO("");
}

bool valueMapGet(ValueMap *map, Value key, Value *value) {
    UNUSED(map);
    UNUSED(key);
    UNUSED(value);
    TODO("");
    return false;
}

bool valueMapSet(VM *vm, ValueMap *map, Value key, Value value) {
    UNUSED(vm);
    UNUSED(map);
    UNUSED(key);
    UNUSED(value);
    TODO("");
    return false;
}

void writeValueArray(VM *vm, ValueArray *array, Value value) {
    UNUSED(vm);
    UNUSED(array);
    UNUSED(value);
    TODO("");
}

int addConst(VM *vm, Chunk *chunk, Value value) {
    UNUSED(vm);
    UNUSED(chunk);
    UNUSED(value);
    TODO("");
    return 0;
}

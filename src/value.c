#include "value.h"
#include "common.h"
#include "vm.h"

#define ALLOC_OBJ(type, objType) (type *)allocObject(vm, sizeof(type), objType)

static Obj *allocObject(VM *vm, size_t size, ObjType type) {
    Obj *obj = (Obj *)vmReallocate(vm, NULL, 0, size);
    memset(obj, 0, size);

    obj->type = type;
    obj->isMarked = false;

    obj->next = vm->objects;
    vm->objects = obj;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %s\n", (void *)obj, size, ObjTypeString(type));
#endif /* ifdef DEBUG_LOG_GC */

    return obj;
}

int getLine(const Chunk *chunk, int offset) {
    int start = 0;
    int end = chunk->lines.cnt - 1;
    int len = end;
    LineInfos lines = chunk->lines;

    for (;;) {
        int mid = (start + end) / 2;
        LineInfo line = lines.items[mid];
        if (offset < line.offset) {
            end = mid - 1;
        } else if (mid == len || offset < lines.items[mid + 1].offset) {
            return line.line;
        } else {
            start = mid + 1;
        }
    }
}

void writeChunk(VM *vm, Chunk *chunk, uint8_t byte, int line) {
    VM_da_append(uint8_t, chunk, byte);

    if (chunk->lines.cnt > 0 &&
        chunk->lines.items[chunk->cnt - 1].line == line) {
        return;
    }

    VM_da_append(LineInfo, &chunk->lines,
                 ((LineInfo){chunk->lines.cnt - 1, line}));
}

int addConst(VM *vm, Chunk *chunk, Value value) {
    writeValueArray(vm, &chunk->constants, value);
    return chunk->constants.cnt - 1;
}

void freeChunk(VM *vm, Chunk *chunk) {
    VM_FREE_ARRAY(uint8_t, chunk->code, chunk->cap);
    VM_FREE_ARRAY(LineInfo, chunk->lines.items, chunk->lines.cap);
    freeValueArray(vm, &chunk->constants);
    *chunk = (Chunk){0};
}

void printValue(Value value) {
    UNUSED(value);
    STUB("");
}

static inline uint32_t hashNumber(double value) {
#ifdef NAN_BOXING
    return hashBits(valueToNum(value));
#else
    return hashBits(*((uint64_t *)&value));
#endif
}

static inline uint32_t hashObject(Obj *obj) {
    switch (obj->type) {
    case OBJ_STRING:   return ((ObjString *)obj)->hash;
    case OBJ_CLASS:    return ((ObjClass *)obj)->name->hash;
    case OBJ_ERROR:    return ((ObjError *)obj)->msg->hash;
    case OBJ_FUNCTION: {
        ObjFn *fn = (ObjFn *)obj;
        return hashNumber(fn->arity) ^ hashNumber(fn->cnt) ^ fn->name->hash;
    }
    case OBJ_INSTANCE: {
        uint32_t klassHash = ((ObjInstance *)obj)->klass->name->hash;
        return klassHash ^ hashBits((uint64_t)(uintptr_t)obj);
    }
    case OBJ_BOUND_METHOD:
    case OBJ_CLOSURE:
    case OBJ_NATIVE:
    case OBJ_UPVALUE:
    case OBJ_ARRAY:
    case OBJ_MAP:
    case OBJ_RANGE:
    case __OBJ_CNT:        VM_UNREACHABLE(); return 0;
    }
}

uint32_t hashValue(Value value) {
#ifdef NAN_BOXING
    if (IS_OBJ(value)) return hashObject(AS_OBJ(value));
    return hashBits(value);
#else
    switch (value.type) {
    case VAL_UNDEFINED: return 1;
    case VAL_EMPTY:     return 0;
    case VAL_NIL:       return 7;
    case VAL_BOOL:      return AS_BOOL(value) ? 3 : 5;
    case VAL_NUMBER:    return hashNumber(AS_NUMBER(value));
    case VAL_OBJ:       return hashObject(AS_OBJ(value));
    default:            VM_UNREACHABLE(); break;
    }
#endif // NAN_BOXING
}

// VALUE_MAP

void freeValueMap(VM *vm, ValueMap *map) {
    VM_FREE_ARRAY(ValueEntry, map->items, map->cap);
    *map = (ValueMap){0};
}

static ValueEntry *findEntry(ValueEntry *entries, int cap, Value key) {
    uint32_t idx = hashValue(key) & (cap - 1);
    ValueEntry *tombstone;

    for (;;) {
        ValueEntry *entry = &entries[idx];
        if (IS_EMPTY(entry->key)) {
            if (IS_NIL(entry->value)) {
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // we found a tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (valuesEqual(key, entry->key)) {
            // we found the key
            return entry;
        }
        idx = (idx + 1) & (cap - 1);
    }
}

bool valueMapContains(ValueMap *map, Value key) {
    if (map->cnt == 0) return false;

    ValueEntry *entry = findEntry(map->items, map->cap, key);
    return !IS_EMPTY(entry->key);
}

bool valueMapGet(ValueMap *map, Value key, Value *value) {
    if (map->cnt == 0) return false;

    ValueEntry *entry = findEntry(map->items, map->cap, key);
    if (IS_EMPTY(entry->key)) return false;

    *value = entry->value;
    return true;
}

static inline void adjustCap(VM *vm, ValueMap *map, int cap) {
    ValueEntry *entries = VM_ALLOC(ValueEntry, cap);
    for (int i = 0; i < cap; i++) {
        entries[i] = (ValueEntry){EMPTY_VAL, NIL_VAL};
    }

    map->cnt = 0;
    for (int i = 0; i < map->cap; i++) {
        ValueEntry *entry = &map->items[i];
        if (IS_EMPTY(entry->key)) continue;

        ValueEntry *dest = findEntry(entries, cap, entry->key);
        *dest = (ValueEntry){entry->key, entry->value};
        map->cnt++;
    }

    VM_FREE_ARRAY(ValueEntry, map->items, map->cap);
    map->items = entries;
    map->cap = cap;
}

bool valueMapSet(VM *vm, ValueMap *map, Value key, Value value) {
    if (map->cnt + 1 > map->cap * MAP_MAX_LOAD) {
        int cap = GROW_CAP(map->cap);
        adjustCap(vm, map, cap);
    }

    ValueEntry *entry = findEntry(map->items, map->cap, key);
    bool isNewKey = IS_EMPTY(entry->key);
    if (isNewKey && IS_NIL(entry->value)) map->cnt++;
    *entry = (ValueEntry){key, value};
    return false;
}

bool valueMapDelete(ValueMap *map, Value key) {
    if (map->cnt == 0) return false;

    // find entry
    ValueEntry *entry = findEntry(map->items, map->cap, key);
    if (IS_EMPTY(entry->key)) return false;

    *entry = (ValueEntry){EMPTY_VAL, BOOL_VAL(true)};
    return true;
}

void valueMapClear(ValueMap *map) {
    for (int i = 0; i < map->cap; i++) {
        map->items[i] = (ValueEntry){EMPTY_VAL, NIL_VAL};
    }
    map->cnt = 0;
}

ObjString *valueMapFindString(ValueMap *map, string str, uint32_t hash) {
    if (map->cnt == 0) return NULL;

    uint32_t idx = hash & (map->cap - 1);
    for (;;) {
        ValueEntry *entry = &map->items[idx];
        if (IS_EMPTY(entry->key)) {
            // stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value)) return NULL;
        } else if (IS_STRING(entry->key)) {
            ObjString *string = AS_STRING(entry->key);
            if (stringsEqual(string->str, str)) {
                // we found it
                return string;
            }
        }
        idx = (idx + 1) & (map->cap - 1);
    }
}

// VALUE_ARRAY
void writeValueArray(VM *vm, ValueArray *array, Value value) {
    VM_da_append(Value, array, value);
}

void freeValueArray(VM *vm, ValueArray *array) {
    VM_FREE_ARRAY(Value, array->items, array->cap);
    *array = (ValueArray){0};
}

// OBJECTS
ObjFn *newObjFn(VM *vm) {
    ObjFn *fn = ALLOC_OBJ(ObjFn, OBJ_FUNCTION);
    *fn = (ObjFn){0};
    return fn;
}

static ObjString *allocString(VM *vm, stringView str, uint32_t hash) {
    ObjString *s = ALLOC_OBJ(ObjString, OBJ_STRING);
    s->cnt = str.cnt;
    s->items = str.items;
    s->hash = hash;

    pushRoot(vm, OBJ_VAL(s));
    valueMapSet(vm, &vm->strings, OBJ_VAL(s), NIL_VAL);
    popRoot(vm);
    return s;
}

ObjString *takeString(VM *vm, char *chars, int length) {
    stringView str = newSv(chars, length);
    uint32_t hash = hashString(str);
    ObjString *interned = valueMapFindString(&vm->strings, str, hash);
    if (interned != NULL) {
        VM_FREE_ARRAY(char, chars, length);
        return interned;
    }
    return allocString(vm, str, hash);
}

ObjString *copyString(VM *vm, const char *chars, int length) {
    stringView str = newSv(chars, length);
    uint32_t hash = hashString(str);
    ObjString *interned = valueMapFindString(&vm->strings, str, hash);
    if (interned != NULL) return interned;
    char *heapChars = VM_ALLOC(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocString(vm, str, hash);
}

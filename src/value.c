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

static inline int fnStrLen(const ObjFn *fn) {
    if (fn->name == NULL) return sizeof("<script>") - 1;
    return fn->name->cnt + ((sizeof("<fn ") - 1) + (sizeof(">") - 1));
}

int objectStringLength(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_ARRAY: {
        ValueArray elms = AS_ARRAY(value)->array;
        if (elms.cnt == 0) return sizeof("[]") - 1;
        // int total = 2 + ((elms.cnt - 1) * 2);
        int total = elms.cnt * 2;
        for (int i = 0; i < elms.cnt; i++) {
            total += valueStringLength(elms.items[i]);
        }
        return total;
    }
    case OBJ_MAP: {
        ValueMap elms = AS_MAP(value)->map;
        if (elms.cnt == 0) return sizeof("{}") - 1;
        // int total = 2 + (elms.cnt * 2) + ((elms.cnt - 1) * 2);
        int total = elms.cnt * 4;
        for (int i = 0; i < elms.cnt; i++) {
            ValueEntry entry = elms.items[i];
            if (IS_EMPTY(entry.key)) continue;

            total += valueStringLength(entry.key);
            total += valueStringLength(entry.value);
        }
        return total;
    }
    case OBJ_UPVALUE: return sizeof("upvalue") - 1;
    case OBJ_NATIVE:  return sizeof("<native fn>") - 1;
    case OBJ_CLASS:   return AS_CLASS(value)->name->cnt;
    case OBJ_INSTANCE:
        return AS_INSTANCE(value)->klass->name->cnt + sizeof(" instance") - 1;
    case OBJ_STRING:       return AS_STRING(value)->cnt;
    case OBJ_ERROR:        return AS_ERROR(value)->msg->cnt;
    case OBJ_CLOSURE:      return fnStrLen(AS_CLOSURE(value)->fn);
    case OBJ_FUNCTION:     return fnStrLen(AS_FUNCTION(value));
    case OBJ_BOUND_METHOD: /*return
                              fnStrLen(AS_BOUND_METHOD(value)->method->fn);*/
    case OBJ_RANGE:        /*{
            const ObjRange *range = AS_RANGE(value);
            return snprintf(NULL, 0, "range(%g, %g, %g)", range->start, range->stop,
                            range->step);
        }*/
    case __OBJ_CNT:
        STUB(ObjTypeString(OBJ_TYPE(value)));
        return -1;
        // case __OBJ_CNT: VM_UNREACHABLE(); return -1;
    }
}

int valueStringLength(Value value) {
#ifdef NAN_BOXING
    if (IS_UNDEFINED(value)) {
        return sizeof("<undefined>") - 1;
    } else if (IS_BOOL(value)) {
        return AS_BOOL(value) ? sizeof("true") - 1 : sizeof("false") - 1;
    } else if (IS_NIL(value)) {
        return sizeof("nil") - 1;
    } else if (IS_NUMBER(value)) {
        return snprintf(NULL, 0, "%.14g", AS_NUMBER(value));
    } else if (IS_EMPTY(value)) {
        return sizeof("<empty>") - 1;
    } else if (IS_OBJ(value)) {
        return objectStringLength(value);
    }
    VM_UNREACHABLE();
    return -1;
#else
    switch (value.type) {
    case VAL_UNDEFINED: return sizeof("<undefined>") - 1;
    case VAL_BOOL:
        return AS_BOOL(value) ? sizeof("true") - 1 : sizeof("false") - 1;
    case VAL_NIL:    return sizeof("nil") - 1;
    case VAL_EMPTY:  return sizeof("<empty>") - 1;
    case VAL_NUMBER: return snprintf(NULL, 0, "%.14g", AS_NUMBER(value));
    case VAL_OBJ:    return objectStringLength(value);
    default:         VM_UNREACHABLE(); return -1;
    }
#endif /* ifdef NAN_BOXING */
}

static inline void printFunc(const ObjFn *fn) {
    if (fn->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", fn->name->items);
}

static void printObject(Value value) {
    ObjType type = OBJ_TYPE(value);
    switch (type) {
    case OBJ_ARRAY: {
        ValueArray elms = AS_ARRAY(value)->array;
        if (elms.cnt == 0) {
            printf("[]");
            return;
        }

        printf("[");
        for (int i = 0; i < elms.cnt; i++) {
            printValue(elms.items[i]);
            if (i != elms.cnt - 1) printf(", ");
        }
        printf("]");
    } break;
    case OBJ_MAP: {
        ValueMap elms = AS_MAP(value)->map;
        if (elms.cnt == 0) {
            printf("{}");
            return;
        }

        printf("{");
        bool first = true;
        for (int i = 0; i < elms.cap; i++) {
            ValueEntry entry = elms.items[i];
            if (IS_EMPTY(entry.key)) continue;
            if (!first) printf(", ");
            first = false;
            printValue(entry.key);
            printf(": ");
            printValue(entry.value);
        }
        printf("}");
    } break;
    case OBJ_INSTANCE:
        printf("%s instance", AS_INSTANCE(value)->klass->name->items);
        break;
    case OBJ_CLASS:    printf("%s", AS_CLASS(value)->name->items); break;
    case OBJ_STRING:   printf("%s", AS_CSTRING(value)); break;
    case OBJ_ERROR:    printf("%s", AS_ERROR_MSG(value)); break;
    case OBJ_NATIVE:   printf("<native fn>"); break;
    case OBJ_UPVALUE:  printf("upvalue"); break;
    case OBJ_CLOSURE:  printFunc(AS_CLOSURE(value)->fn); break;
    case OBJ_FUNCTION: printFunc(AS_FUNCTION(value)); break;
    case OBJ_BOUND_METHOD:
        /* printFunc(AS_BOUND_METHOD(value)->method->fn); break; */
    case OBJ_RANGE: /*{
     const ObjRange *range = AS_RANGE(value);
     printf("range(%g, %g, %g)", range->start, range->stop, range->step);
 } break; */
    case __OBJ_CNT:
        STUB(ObjTypeString(type));
        break;
        // case __OBJ_CNT: VM_UNREACHABLE(); break;
    }
}

void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        printf("nil");
    } else if (IS_EMPTY(value)) {
        printf("<empty>");
    } else if (IS_UNDEFINED(value)) {
        printf("<undefined>");
    } else if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        printObject(value);
    }
#else
    switch (value.type) {
    case VAL_UNDEFINED: printf("<undefined>"); break;
    case VAL_EMPTY:     printf("<empty>"); break;
    case VAL_NIL:       printf("nil"); break;
    case VAL_BOOL:      printf(AS_BOOL(value) ? "true" : "false"); break;
    case VAL_NUMBER:    printf("%g", AS_NUMBER(value)); break;
    case VAL_OBJ:       printObject(value); break;
    }
#endif /* ifdef NAN_BOXING */
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
    case __OBJ_CNT:
    default:               VM_UNREACHABLE(); return 0;
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
    ValueEntry *tombstone = NULL;

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

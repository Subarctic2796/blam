#ifndef INCLUDE_SRC_VALUE_H_
#define INCLUDE_SRC_VALUE_H_

typedef enum {
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_ERROR,
    OBJ_UPVALUE,
    OBJ_ARRAY,
    OBJ_MAP,
    OBJ_RANGE,
} ObjType;

static inline const char *ObjTypeString(ObjType t) {
    static const char *strings[] = {
        [OBJ_BOUND_METHOD] = "OBJ_BOUND_METHOD",
        [OBJ_CLASS] = "OBJ_CLASS",
        [OBJ_CLOSURE] = "OBJ_CLOSURE",
        [OBJ_FUNCTION] = "OBJ_FUNCTION",
        [OBJ_INSTANCE] = "OBJ_INSTANCE",
        [OBJ_NATIVE] = "OBJ_NATIVE",
        [OBJ_STRING] = "OBJ_STRING",
        [OBJ_ERROR] = "OBJ_ERROR",
        [OBJ_UPVALUE] = "OBJ_UPVALUE",
        [OBJ_ARRAY] = "OBJ_ARRAY",
        [OBJ_MAP] = "OBJ_MAP",
        [OBJ_RANGE] = "OBJ_RANGE",
    };
    return strings[t];
}

typedef struct Obj {
    ObjType type;
    bool isMarked;
    struct Obj *next;
} Obj;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)1 << 63)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1 // 001
#define TAG_FALSE 2 // 010
#define TAG_TRUE  3 // 011
#define TAG_EMPTY 4 // 100
#define TAG_EMPTY 5 // 101

typedef uint64_t Value;

// check lox type is correct c type
#define IS_BOOL(value)   (((value) | 1) == TRUE_VAL)
#define IS_NIL(value)    ((value) == NIL_VAL)
#define IS_EMPTY(value)  ((value) == EMPTY_VAL)
#define IS_NUMBER(value) (((value) & QNAN) != QNAN)
#define IS_OBJ(value)    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

// lox -> c
#define AS_BOOL(value)   ((value) == TRUE_VAL)
#define AS_NUMBER(value) valueToNum(value)
#define AS_OBJ(value)    ((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

// c -> lox
#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define EMPTY_VAL       ((Value)(uint64_t)(QNAN | TAG_EMPTY))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj)    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

#else

typedef enum {
    VAL_UNDEFINED,
    VAL_EMPTY,
    VAL_NIL,
    VAL_BOOL,
    VAL_NUM,
    VAL_OBJ,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool bool_;
        double num;
        Obj *obj;
    } as;
} Value;

// check lox type is correct c type
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_EMPTY(value)   ((value).type == VAL_EMPTY)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

// lox -> c
#define AS_OBJ(value)     ((value).as.obj)
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)

// c -> lox
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define EMPTY_VAL         ((Value){VAL_EMPTY, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj *)object}})
#endif // NAN_BOXING

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)        isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)     isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)
#define IS_ERROR(value)        isObjType(value, OBJ_ERROR)
#define IS_ARRAY(value)        isObjType(value, OBJ_ARRAY)
#define IS_MAP(value)          isObjType(value, OBJ_MAP)
#define IS_RANGE(value)        isObjType(value, OBJ_RANGE)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value)        ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value)      ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFn *)AS_OBJ(value))
#define AS_INSTANCE(value)     ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value)       (((ObjNative *)AS_OBJ(value))->function)
#define AS_ARRAY(value)        ((ObjArray *)AS_OBJ(value))
#define AS_MAP(value)          ((ObjMap *)AS_OBJ(value))
#define AS_RANGE(value)        ((ObjRange *)AS_OBJ(value))
#define AS_STRING(value)       ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString *)AS_OBJ(value))->chars)
#define AS_ERROR(value)        ((ObjError *)AS_OBJ(value))
#define AS_ERROR_MSG(value)    (((ObjError *)AS_OBJ(value))->msg->chars)

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif // INCLUDE_SRC_VALUE_H_

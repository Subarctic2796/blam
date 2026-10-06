#include "vm.h"
#include "ast.h"
#include "common.h"
#include "compiler.h"
#include "parser.h"
#include "value.h"

static AstPrettyPrinter AST_PRINTER = {0};

static void collectGarbage(VM *vm);

void *vmReallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize) {
    vm->bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage(vm);
#endif

        if (vm->bytesAllocated > vm->nextGC) collectGarbage(vm);
    }

    void *result = realloc(ptr, newSize);
    if (result == NULL) UNREACHABLE("buy more memory lol");
    return result;
}

static void freeObject(VM *vm, Obj *object) {
#ifdef DEBUG_LOG_GC
    printf("%p free %s ", (void *)object, ObjTypeString(object->type));
    printValue(OBJ_VAL(object));
    puts("");
#endif // ifdef DEBUG_LOG_GC

    UNUSED(vm);

    switch (object->type) {
    case OBJ_BOUND_METHOD:
    case OBJ_CLASS:
    case OBJ_CLOSURE:
    case OBJ_FUNCTION:
    case OBJ_INSTANCE:
    case OBJ_NATIVE:
    case OBJ_STRING:
    case OBJ_ERROR:
    case OBJ_UPVALUE:
    case OBJ_ARRAY:
    case OBJ_MAP:
    case OBJ_RANGE:
    case __OBJ_CNT:        STUB(ObjTypeString(object->type)); break;
    }
}

static void freeObjects(VM *vm) {
    Obj *object = vm->objects;
    while (object != NULL) {
        Obj *next = object->next;
        freeObject(vm, object);
        object = next;
    }

    free(vm->grayStack);
}

static void collectGarbage(VM *vm) {
    UNUSED(vm);
    STUB("");
}

void initVM(VM *vm, Parser *parser, Compiler *compiler) {
    *vm = (VM){0};

    initParser(parser);
    initCompiler(compiler);

    vm->parser = parser;
    vm->compiler = compiler;
}

void freeVM(VM *vm) {
    arena_free(&AST_PRINTER.arena);
    freeParser(vm->parser);
    freeCompiler(vm->compiler);
    freeObjects(vm);
    STUB("");
}

InterpretResult interpret(VM *vm, const char *src) {
    if (!parse(vm->parser, src)) return INTERPRET_COMPILE_ERR;

    Stmts stmts = parserGetStmts(vm->parser);
    astPrettyPrint(&AST_PRINTER, stmts);

    ObjFn *func = compile(vm, vm->compiler, stmts);
    if (func == NULL) return INTERPRET_COMPILE_ERR;

    return INTERPRET_OK;
}

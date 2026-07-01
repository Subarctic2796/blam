#include <stdio.h>

#include <readline/history.h>
#include <readline/readline.h>

#define COMMON_IMPLEMENTATION
#include "common.h"

#include "ast.h"
#include "parser.h"
#include "resolver.h"
#include "vm.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

static inline void repl(VM *vm) {
    UNUSED(vm);

    char *line = NULL;
    Stmts stmts = {0};
    for (;;) {
        if (line != NULL) free(line);
        line = readline("> ");
        if (line == NULL) break;
        add_history(line);

        interpret(vm, line, &stmts);
    }
}

static inline void runFile(VM *vm, const char *path) {
    UNUSED(vm);
    UNUSED(path);
    TODO("runFile");

    string src = readFile(path);
    Stmts stmts = {0};
    InterpretResult result = interpret(vm, src.items, &stmts);
    free_string(src);

    switch (result) {
    case INTERPRET_COMPILE_ERR: exit(65);
    case INTERPRET_RUNTIME_ERR: exit(70);
    case INTERPRET_OK:          return;
    default:
        fprintf(stderr, "[unreachable] can't have any other VM exit code\n");
        exit(64);
    }
}

int main(int argc, char *argv[]) {
    uint8_t PARSER_BUFFER[PARSER_SIZE] = {0};
    Parser *parser = (Parser *)PARSER_BUFFER;

    uint8_t RESOLVER_BUFFER[RESOLVER_SIZE] = {0};
    Resolver *resolver = (Resolver *)RESOLVER_BUFFER;

    void *compiler = NULL;
    VM vm = {0};
    initVM(&vm, parser, resolver, &compiler);

    switch (argc) {
    case 1:  repl(&vm); break;
    case 2:  runFile(&vm, argv[1]); break;
    default: fprintf(stderr, "Usage: blam [script]\n"); break;
    }

    freeVM(&vm);
    return EXIT_SUCCESS;
}

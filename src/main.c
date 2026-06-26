#include <stdio.h>

#include <readline/history.h>
#include <readline/readline.h>

#define COMMON_IMPLEMENTATION
#include "common.h"

#include "ast.h"
#include "parser.h"
#include "vm.h"

static inline void repl(VM *vm) {
    UNUSED(vm);

    char *line = NULL;
    Stmts stmts = {0};
    for (;;) {
        if (line != NULL) free(line);
        line = readline("> ");
        if (line == NULL) break;

        clearStmts(&stmts);

        resetParser(vm->parser, line);
        if (!parse(vm->parser, &stmts)) continue;
        for (size_t i = 0; i < stmts.cnt; i++) {
            printStmt(stmts.items[i]);
            puts("");
        }
    }

    freeStmts(&stmts);
}

static inline void runFile(VM *vm, const char *path) {
    UNUSED(vm);
    UNUSED(path);
    TODO("runFile");
}

int main(int argc, char *argv[]) {
    Parser parser = {0};
    void *compiler = NULL;
    VM vm = {0};
    initVM(&vm, &parser, &compiler);

    switch (argc) {
    case 1:  repl(&vm); break;
    case 2:  runFile(&vm, argv[1]); break;
    default: fprintf(stderr, "Usage: blam [script]\n"); break;
    }

    freeVM(&vm);
    return EXIT_SUCCESS;
}

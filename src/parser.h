#ifndef INCLUDE_SRC_PARSER_H_
#define INCLUDE_SRC_PARSER_H_

#include "ast.h"
#include "common.h"
#include "token.h"

typedef struct {
    const char *start;
    const char *cur;
    size_t line;
} Lexer;

typedef struct {
    bool hadErr;
    Lexer lexer;
} Parser;

void resetParser(Parser *p, const char *src);
Token scanToken(Parser *p);

bool parse(Parser *p, Stmts *stmts);

#endif // INCLUDE_SRC_PARSER_H_

#ifndef INCLUDE_SRC_PARSER_H_
#define INCLUDE_SRC_PARSER_H_

#include "ast.h"
#include "common.h"
#include "token.h"

typedef struct VM VM;

#define PARSER_SIZE 160
typedef struct Parser Parser;

void initParser(Parser *p);
void freeParser(Parser *p);
bool parse(VM *vm, Parser *p, const char *src, Stmts *stmts);

#endif // INCLUDE_SRC_PARSER_H_

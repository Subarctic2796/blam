#ifndef INCLUDE_SRC_PARSER_H_
#define INCLUDE_SRC_PARSER_H_

#include "ast.h"
#include "common.h"
#include "token.h"

#define PARSER_SIZE 176
typedef struct Parser Parser;

void initParser(Parser *p);
void freeParser(Parser *p);
bool parse(Parser *p, const char *src);
Stmts parserGetStmts(const Parser *p);

#endif // INCLUDE_SRC_PARSER_H_

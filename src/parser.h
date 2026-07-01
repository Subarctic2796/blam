#ifndef INCLUDE_SRC_PARSER_H_
#define INCLUDE_SRC_PARSER_H_

#include "ast.h"
#include "common.h"
#include "token.h"

#define PARSER_SIZE 112

typedef struct Parser Parser;

void initParser(Parser *p);
void resetParser(Parser *p, const char *src);
void freeParser(Parser *p);
bool parse(Parser *p, Stmts *stmts);

#endif // INCLUDE_SRC_PARSER_H_

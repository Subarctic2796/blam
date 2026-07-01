#ifndef INCLUDE_SRC_RESOLVER_H_
#define INCLUDE_SRC_RESOLVER_H_

#include "ast.h"
#include "common.h"

#define RESOLVER_SIZE 96

typedef struct Resolver Resolver;

void initResolver(Resolver *r);
void freeResolver(Resolver *r);
void resetResolver(Resolver *r);
bool resolve(Resolver *r, Stmts *stmts);

#endif // INCLUDE_SRC_RESOLVER_H_

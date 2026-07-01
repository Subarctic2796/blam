#ifndef INCLUDE_SRC_LEXER_H_
#define INCLUDE_SRC_LEXER_H_

#include "common.h"
#include "token.h"

typedef struct {
    const char *start;
    const char *cur;
    size_t line;
} Lexer;

static inline void initLexer(Lexer *l, const char *src) {
    *l = (Lexer){src, src, 1};
}
Token scanToken(Lexer *lexer);

#endif // INCLUDE_SRC_LEXER_H_

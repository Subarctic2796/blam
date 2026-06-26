#ifndef INCLUDE_SRC_TOKEN_H_
#define INCLUDE_SRC_TOKEN_H_

#include "common.h"

typedef enum {
    // Single-character tokens.
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LSQR,
    TOKEN_RSQR,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_PERCENT,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_NEQ,
    TOKEN_EQ,
    TOKEN_EQEQ,
    TOKEN_GT,
    TOKEN_GTEQ,
    TOKEN_LT,
    TOKEN_LTEQ,
    TOKEN_PLUS_EQ,
    TOKEN_MINUS_EQ,
    TOKEN_SLASH_EQ,
    TOKEN_STAR_EQ,
    TOKEN_PERCENT_EQ,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_IN,

    TOKEN_ERROR,
    TOKEN_EOF,
    __TOKEN_CNT,
} TokenType;

static inline const char *tokenTypeString(TokenType t) {
    static const char *strings[] = {
        "TOKEN_LPAREN",     // TOKEN_LPAREN
        "TOKEN_RPAREN",     // TOKEN_RPAREN
        "TOKEN_LBRACE",     // TOKEN_LBRACE
        "TOKEN_RBRACE",     // TOKEN_RBRACE
        "TOKEN_LSQR",       // TOKEN_LSQR
        "TOKEN_RSQR",       // TOKEN_RSQR
        "TOKEN_COMMA",      // TOKEN_COMMA
        "TOKEN_DOT",        // TOKEN_DOT
        "TOKEN_SEMICOLON",  // TOKEN_SEMICOLON
        "TOKEN_COLON",      // TOKEN_COLON
        "TOKEN_MINUS",      // TOKEN_MINUS
        "TOKEN_PLUS",       // TOKEN_PLUS
        "TOKEN_SLASH",      // TOKEN_SLASH
        "TOKEN_STAR",       // TOKEN_STAR
        "TOKEN_PERCENT",    // TOKEN_PERCENT
        "TOKEN_BANG",       // TOKEN_BANG
        "TOKEN_NEQ",        // TOKEN_NEQ
        "TOKEN_EQ",         // TOKEN_EQ
        "TOKEN_EQEQ",       // TOKEN_EQEQ
        "TOKEN_GT",         // TOKEN_GT
        "TOKEN_GTEQ",       // TOKEN_GTEQ
        "TOKEN_LT",         // TOKEN_LT
        "TOKEN_LTEQ",       // TOKEN_LTEQ
        "TOKEN_PLUS_EQ",    // TOKEN_PLUS_EQ
        "TOKEN_MINUS_EQ",   // TOKEN_MINUS_EQ
        "TOKEN_SLASH_EQ",   // TOKEN_SLASH_EQ
        "TOKEN_STAR_EQ",    // TOKEN_STAR_EQ
        "TOKEN_PERCENT_EQ", // TOKEN_PERCENT_EQ
        "TOKEN_IDENTIFIER", // TOKEN_IDENTIFIER
        "TOKEN_STRING",     // TOKEN_STRING
        "TOKEN_NUMBER",     // TOKEN_NUMBER
        "TOKEN_AND",        // TOKEN_AND
        "TOKEN_CLASS",      // TOKEN_CLASS
        "TOKEN_ELSE",       // TOKEN_ELSE
        "TOKEN_FALSE",      // TOKEN_FALSE
        "TOKEN_FOR",        // TOKEN_FOR
        "TOKEN_FUN",        // TOKEN_FUN
        "TOKEN_IF",         // TOKEN_IF
        "TOKEN_NIL",        // TOKEN_NIL
        "TOKEN_OR",         // TOKEN_OR
        "TOKEN_PRINT",      // TOKEN_PRINT
        "TOKEN_RETURN",     // TOKEN_RETURN
        "TOKEN_SUPER",      // TOKEN_SUPER
        "TOKEN_THIS",       // TOKEN_THIS
        "TOKEN_TRUE",       // TOKEN_TRUE
        "TOKEN_VAR",        // TOKEN_VAR
        "TOKEN_WHILE",      // TOKEN_WHILE
        "TOKEN_BREAK",      // TOKEN_BREAK
        "TOKEN_CONTINUE",   // TOKEN_CONTINUE
        "TOKEN_IN",         // TOKEN_IN
        "TOKEN_ERROR",      // TOKEN_ERROR
        "TOKEN_EOF",        // TOKEN_EOF
    };
    static_assert(ARRAY_LEN(strings) == __TOKEN_CNT,
                  "number of tokens changed");
    return strings[t];
};

typedef struct {
    TokenType type;
    int line;
    union {
        struct {
            const char *items;
            size_t cnt;
        };
        stringView lexeme;
    };
} Token;

typedef struct {
    Token *items;
    size_t cnt, cap;
} Tokens;

static inline void printToken(Token t) {
    printf("%s %d %.*s", tokenTypeString(t.type), t.line, (int)t.cnt, t.items);
}

#endif // INCLUDE_SRC_TOKEN_H_

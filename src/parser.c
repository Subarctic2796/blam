#include "parser.h"
#include "ast.h"
#include "common.h"

static inline bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
static inline bool isDigit(char c) { return c >= '0' && c <= '9'; }

static inline bool isAtEnd(Lexer *l) { return *l->cur == '\0'; }
static inline char lexerAdvance(Lexer *l) { return *(l->cur++); }
static inline char lexerPeek(Lexer *l) { return *l->cur; }
static inline char lexerPeekNext(Lexer *l) {
    return isAtEnd(l) ? '\0' : l->cur[1];
}

static inline bool lexerMatch(Lexer *l, char expected) {
    if (isAtEnd(l)) return false;
    if (*l->cur != expected) return false;
    l->cur++;
    return true;
}

static inline Token makeToken(Lexer *l, TokenType type) {
    return (Token){
        .type = type,
        .line = l->line,
        .lexeme = {l->start, (int)(l->cur - l->start)},
    };
}

static inline Token matchMakeToken(Lexer *l, char expected, TokenType t1,
                                   TokenType t2) {
    return makeToken(l, lexerMatch(l, expected) ? t1 : t2);
}

static inline Token errorToken(Lexer *l, const char *msg) {
    return (Token){
        .type = TOKEN_ERROR,
        .line = l->line,
        .lexeme = {msg, (int)strnlen(msg, 1024)},
    };
}

static inline Token skipWhiteSpace(Lexer *l) {
    for (;;) {
        char c = lexerPeek(l);
        switch (c) {
        case ' ':
        case '\r':
        case '\t': lexerAdvance(l); break;
        case '\n':
            l->line++;
            lexerAdvance(l);
            break;
        case '/':
            if (lexerPeekNext(l) == '/') {
                while (lexerPeek(l) != '\n' && !isAtEnd(l)) {
                    lexerAdvance(l);
                }
            } else if (lexerPeekNext(l) == '*') {
                lexerAdvance(l);
                int nesting = 1;
                while (nesting > 0) {
                    if (isAtEnd(l)) {
                        return errorToken(l, "Unterminated block comment");
                    }

                    if (lexerPeek(l) == '/' && lexerPeekNext(l) == '*') {
                        lexerAdvance(l);
                        lexerAdvance(l);
                        nesting++;
                        continue;
                    }

                    if (lexerPeek(l) == '*' && lexerPeekNext(l) == '/') {
                        lexerAdvance(l);
                        lexerAdvance(l);
                        nesting--;
                        continue;
                    }

                    lexerAdvance(l);
                }
            } else {
                return (Token){0};
            }
            break;
        default: return (Token){0};
        }
    }
}

static inline TokenType identifierType(Lexer *l) {
    int len = l->cur - l->start;
    if ((len < 1) || (len > 8)) return TOKEN_IDENTIFIER;

    typedef struct {
        TokenType type;
        const char *chars;
        int len;
    } Keyword;

    static const Keyword KEYWORDS[] = {
        {TOKEN_IF, "if", 2},
        {TOKEN_IN, "in", 2},
        {TOKEN_OR, "or", 2},
        {TOKEN_AND, "and", 3},
        {TOKEN_FOR, "for", 3},
        {TOKEN_FUN, "fun", 3},
        {TOKEN_NIL, "nil", 3},
        {TOKEN_VAR, "var", 3},
        {TOKEN_ELSE, "else", 4},
        {TOKEN_THIS, "this", 4},
        {TOKEN_TRUE, "true", 4},
        {TOKEN_BREAK, "break", 5},
        {TOKEN_CLASS, "class", 5},
        {TOKEN_FALSE, "false", 5},
        {TOKEN_PRINT, "print", 5},
        {TOKEN_SUPER, "super", 5},
        {TOKEN_WHILE, "while", 5},
        {TOKEN_RETURN, "return", 6},
        {TOKEN_CONTINUE, "continue", 8},
    };
#define NUM_KEYWORDS 19
    static_assert((sizeof KEYWORDS / sizeof KEYWORDS[0]) == NUM_KEYWORDS,
                  "number of keywords changed");

    for (size_t i = 0; i < NUM_KEYWORDS; i++) {
        Keyword kw = KEYWORDS[i];
        if (len == kw.len && memcmp(l->start, kw.chars, len) == 0) {
            return kw.type;
        }
    }
    return TOKEN_IDENTIFIER;
}

static inline Token makeIdent(Lexer *l) {
    while (isAlpha(lexerPeek(l)) || isDigit(lexerPeek(l))) {
        lexerAdvance(l);
    }
    return makeToken(l, identifierType(l));
}

static inline Token makeNumber(Lexer *l) {
    while (isDigit(lexerPeek(l))) {
        lexerAdvance(l);
    }

    // look for fractional part
    if (lexerPeek(l) == '.' && isDigit(lexerPeekNext(l))) {
        // consume '.'
        lexerAdvance(l);

        while (isDigit(lexerPeek(l))) {
            lexerAdvance(l);
        }
    }
    return makeToken(l, TOKEN_NUMBER);
}

static inline Token makeString(Lexer *l) {
    while (lexerPeek(l) != '"' && !isAtEnd(l)) {
        if (lexerPeek(l) == '\n') l->line++;
        lexerAdvance(l);
    }
    if (isAtEnd(l)) return errorToken(l, "Unterminated string");
    // the closing quote
    lexerAdvance(l);
    return makeToken(l, TOKEN_STRING);
}

void resetParser(Parser *p, const char *src) {
    p->hadErr = false;
    p->lexer = (Lexer){src, src, 1};
}

Token scanToken(Parser *parser) {
    Lexer *lexer = &parser->lexer;
    Token err = skipWhiteSpace(lexer);
    if (err.type == TOKEN_ERROR) return err;

    lexer->start = lexer->cur;

    if (isAtEnd(lexer)) return makeToken(lexer, TOKEN_EOF);

    char c = lexerAdvance(lexer);
    if (isAlpha(c)) return makeIdent(lexer);
    if (isDigit(c)) return makeNumber(lexer);

    switch (c) {
    case '(': return makeToken(lexer, TOKEN_LPAREN);
    case ')': return makeToken(lexer, TOKEN_RPAREN);
    case '{': return makeToken(lexer, TOKEN_LBRACE);
    case '}': return makeToken(lexer, TOKEN_RBRACE);
    case '[': return makeToken(lexer, TOKEN_LSQR);
    case ']': return makeToken(lexer, TOKEN_RSQR);
    case ';': return makeToken(lexer, TOKEN_SEMICOLON);
    case ':': return makeToken(lexer, TOKEN_COLON);
    case ',': return makeToken(lexer, TOKEN_COMMA);
    case '.': return makeToken(lexer, TOKEN_DOT);
    case '-': return matchMakeToken(lexer, '=', TOKEN_MINUS_EQ, TOKEN_MINUS);
    case '/': return matchMakeToken(lexer, '=', TOKEN_SLASH_EQ, TOKEN_SLASH);
    case '*': return matchMakeToken(lexer, '=', TOKEN_STAR_EQ, TOKEN_STAR);
    case '+': return matchMakeToken(lexer, '=', TOKEN_PLUS_EQ, TOKEN_PLUS);
    case '%':
        return matchMakeToken(lexer, '=', TOKEN_PERCENT_EQ, TOKEN_PERCENT);
    case '!': return matchMakeToken(lexer, '=', TOKEN_NEQ, TOKEN_BANG);
    case '=': return matchMakeToken(lexer, '=', TOKEN_EQEQ, TOKEN_EQ);
    case '<': return matchMakeToken(lexer, '=', TOKEN_LTEQ, TOKEN_LT);
    case '>': return matchMakeToken(lexer, '=', TOKEN_GTEQ, TOKEN_GT);
    case '"': return makeString(lexer);
    }

    return errorToken(lexer, "Unexpected character");
}

bool parse(Parser *p, Stmts *stmts) {
    UNUSED(p);
    UNUSED(stmts);
    TODO("");
    return !p->hadErr;
}

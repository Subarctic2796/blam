#include "resolver.h"
#include "arena.h"
#include "common.h"
#include "token.h"

typedef struct {
    string *items;
    size_t cnt, cap;
} StrSet;

static string *strSetFindStr(StrSet *s, string key) {
    uint32_t idx = hashString(key) & (s->cap);
    string *tombstone = NULL;

    for (;;) {
        string *entry = &s->items[idx];
        if (entry->items == NULL) {
            if (entry->cnt != 0) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (stringsEqual(key, *entry)) {
            return entry;
        }

        idx = (idx + 1) & (s->cap - 1);
    }
}

typedef enum {
    VS_NONE,
    VS_DECLARED,
    VS_DEFINED,
    VS_IMPLICIT,
} VarStatus;

typedef struct {
    VarStatus value;
    Token key;
} ScopeEntry;

typedef struct {
    ScopeEntry *items;
    size_t cnt, cap;
} Scope;

static ScopeEntry *findScopeEntry(ScopeEntry *entries, int cap, Token key) {
    uint32_t idx = hashString(key.lexeme) & (cap - 1);
    ScopeEntry *tombstone = NULL;

    for (;;) {
        ScopeEntry *entry = &entries[idx];
        if (stringEmpty(entry->key.lexeme)) {
            if (entry->value == VS_NONE) {
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // found a tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (stringsEqual(key.lexeme, entry->key.lexeme)) {
            // found the key
            return entry;
        }

        idx = (idx + 1) & (cap - 1);
    }
}

static void scopeClear(Scope *s) {
    for (size_t i = 0; i < s->cnt; i++) {
        s->items[i] = (ScopeEntry){0};
    }
    s->cnt = 0;
}

static bool scopeContains(Scope *s, Token key) {
    if (s->cnt == 0) return false;
    ScopeEntry *entry = findScopeEntry(s->items, s->cap, key);
    return !stringEmpty(entry->key.lexeme);
}

static bool scopeGet(Scope *s, Token key, VarStatus *value) {
    if (s->cnt == 0) return false;

    ScopeEntry *entry = findScopeEntry(s->items, s->cap, key);
    if (stringEmpty(entry->key.lexeme)) return false;

    *value = entry->value;
    return true;
}

static inline void scopeAdjustCap(Arena *a, Scope *s, size_t cap) {
    ScopeEntry *entries = arena_alloc(a, sizeof(*entries) * cap);
    for (size_t i = 0; i < cap; i++) {
        entries[i] = (ScopeEntry){0};
    }

    s->cnt = 0;
    for (size_t i = 0; i < s->cap; i++) {
        ScopeEntry *entry = &s->items[i];
        if (stringEmpty(entry->key.lexeme)) continue;

        ScopeEntry *dest = findScopeEntry(s->items, cap, entry->key);
        *dest = (ScopeEntry){entry->value, entry->key};
        s->cnt++;
    }
    s->items = entries;
    s->cap = cap;
}

static bool scopeSet(Arena *a, Scope *s, Token key, VarStatus value) {
    if (s->cnt + 1 > s->cap * HASH_TYPES_MAX_LOAD) {
        size_t cap = GROW_CAP(s->cap);
        scopeAdjustCap(a, s, cap);
    }

    ScopeEntry *entry = findScopeEntry(s->items, s->cap, key);
    bool isNewKey = stringEmpty(entry->key.lexeme);
    if (isNewKey && entry->value == VS_NONE) s->cnt++;

    *entry = (ScopeEntry){value, key};
    return isNewKey;
}

typedef struct {
    Scope *items;
    size_t cnt, cap;
} Scopes;

typedef struct Resolver {
    bool hadErr;
    int maxScopes, scopeDepth;
    Arena globalsStrings;
    Arena scopesArena;
    Scopes scopes;
    StrSet globals;
} Resolver;

static_assert(RESOLVER_SIZE == sizeof(Resolver), "size of resolver changed");

// -------
// HELPERS
// -------

static void error(Resolver *r, const char *msg) {
    UNUSED(r);
    UNUSED(msg);
    TODO("");
}

static inline Scope *curScope(Resolver *r) {
    return &r->scopes.items[r->scopes.cnt - 1];
}

static inline bool curScopeIsGlobal(Resolver *r) { return r->scopes.cnt == 1; }

static inline void beginScope(Resolver *r) {
    r->scopeDepth++;
    if (r->maxScopes < (int)r->scopes.cnt + 1) {
        arena_da_append(&r->scopesArena, &r->scopes, ((Scope){0}));
        r->maxScopes = r->scopes.cnt + 1;
    } else {
        r->scopes.cnt++;
    }
}

static inline void endScope(Resolver *r) {
    r->scopeDepth--;
    scopeClear(curScope(r));
    r->scopes.cnt--;
}

static void declare(Resolver *r, Token name) {
    // TODO: make sure the string gets copied if need be
    Scope *cur = curScope(r);
    if (scopeContains(cur, name)) {
        error(r, "Already a variable with this name in the scope");
    }
    scopeSet(&r->scopesArena, cur, name, VS_DECLARED);
}

static void define(Resolver *p, Token name) {
    scopeSet(&p->scopesArena, curScope(p), name, VS_DEFINED);
}

static void declareAndDefine(Resolver *r, Token name) {
    // TODO: make sure the string gets copied if need be
    Scope *cur = curScope(r);
    if (scopeContains(cur, name)) {
        error(r, "Already a variable with this name in the scope");
    }
    scopeSet(&r->scopesArena, cur, name, VS_DEFINED);
}

// TODO: probably have to heap allocate the string
static inline Token syntheticToken(stringView sv) {
    Token token = {0};
    token.lexeme = sv;
    return token;
}

// ---------
// RESOLVING
// ---------

static void resolveExpr(Resolver *r, Expr *expr) {
    UNUSED(r);
    UNUSED(expr);
}

static void resolveStmt(Resolver *r, Stmt *stmt) {
    UNUSED(r);
    UNUSED(stmt);
}

bool resolve(Resolver *r, Stmts *stmts) {
    for (size_t i = 0; i < stmts->cnt; i++) {
        resolveStmt(r, stmts->items[i]);
    }
    return !r->hadErr;
}

void initResolver(Resolver *r) {
    *r = (Resolver){0};
    arena_da_append(&r->scopesArena, &r->scopes, ((Scope){0}));
    r->maxScopes = 1;
}

void freeResolver(Resolver *r) {
    arena_free(&r->scopesArena);
    arena_free(&r->globalsStrings);
}

void resetResolver(Resolver *r) {
    r->hadErr = false;
    r->scopeDepth = 0;
    assert(r->scopes.cnt == 1 && "len of scopes is not 1");
    r->scopes.cnt = 1;
}

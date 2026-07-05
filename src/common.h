#ifndef INCLUDE_MYLIB_MYLIB_H_
#define INCLUDE_MYLIB_MYLIB_H_

// lots of things are directly lifted from tsoding's
// [nob.h](https://github.com/tsoding/nob.h)

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_UPVALUES 256
#define MAX_LOCALS   256

#define API

#define UNUSED(arg) ((void)arg)

#define TODO(message)                                                          \
    do {                                                                       \
        fprintf(stderr, "[%s:%d] in %s() TODO: %s\n", __FILE__, __LINE__,      \
                __func__, strlen(message) == 0 ? __func__ : message);          \
        abort();                                                               \
    } while (0)

#define UNREACHABLE(message)                                                   \
    do {                                                                       \
        fprintf(stderr, "[%s:%d] in %s() UNREACHABLE: %s\n", __FILE__,         \
                __LINE__, __func__, message);                                  \
        abort();                                                               \
    } while (0)

#define ARRAY_LEN(array) (sizeof(array) / sizeof(array[0]))

#ifndef DA_FREE
#define DA_FREE free
#endif

#ifndef DA_REALLOC
#define DA_REALLOC realloc
#endif

#define DA_INIT_CAP 16

#define GROW_CAP(cap) ((cap) < DA_INIT_CAP ? DA_INIT_CAP : (cap) * 2)

#define da_free(da) DA_FREE((da).items)

#define da_reserve(da, expected_capacity)                                      \
    do {                                                                       \
        if ((expected_capacity) > (da)->cap) {                                 \
            if ((da)->cap < DA_INIT_CAP) (da)->cap = DA_INIT_CAP;              \
            while ((expected_capacity) > (da)->cap) {                          \
                (da)->cap *= 2;                                                \
            }                                                                  \
            (da)->items =                                                      \
                DA_REALLOC((da)->items, (da)->cap * sizeof(*(da)->items));     \
            assert((da)->items != NULL && "Buy more RAM lol");                 \
        }                                                                      \
    } while (0)

// Append an item to a dynamic array
#define da_append(da, item)                                                    \
    do {                                                                       \
        da_reserve((da), (da)->cnt + 1);                                       \
        (da)->items[(da)->cnt++] = (item);                                     \
    } while (0)

// Append several items to a dynamic array
#define da_append_many(da, new_items, new_items_count)                         \
    do {                                                                       \
        da_reserve((da), (da)->cnt + (new_items_count));                       \
        memcpy((da)->items + (da)->cnt, (new_items),                           \
               (new_items_count) * sizeof(*(da)->items));                      \
        (da)->cnt += (new_items_count);                                        \
    } while (0)

#define free_string(s) free((void *)(s).items)

#define HASH_TYPES_MAX_LOAD 0.75

// need to free
typedef struct {
    const char *items;
    size_t cnt;
} string;

#define strLit(s)          ((string){s, sizeof(s) - 1})
#define newStr(items, cnt) ((string){(items), (cnt)})

// a stringView is just a typedef of string allowing you to use the same methods
// on it as you would a string but don't need to free it
// TODO: not sure it this should be a typedef of a string as it means that you
// can freely cast between them it also means that some of the semantics may be
// abused as strings are supposed to be heap allocated and therefore, need to be
// freed, while a stringView doesn't this means that if you have a string a
// treat it as a stringView then you may forget to free it
typedef string stringView;

#define svLit(s)          ((stringView){s, sizeof(s) - 1})
#define newSv(items, cnt) ((stringView){(items), (cnt)})

// need to free
typedef struct {
    char *items;
    size_t cnt, cap;
} stringBuilder;

typedef struct {
    stringView *items;
    size_t cnt, cap;
} stringViews;

typedef struct {
    stringView *items;
    size_t cnt, cap;
} strings;

API bool stringEmpty(string s);
API bool stringsEqual(string a, string b);
// returns a copy of the string
API string stringCopy(const string s);
// this needs to be freed
// cstr has to be null terminated
// adds null terminator
API string stringFromCstr(const char *cstr);
// needs to be freed
// adds null terminator
API string stringFromFormat(const char *fmt, ...);
API int sbAppendf(stringBuilder *sb, const char *fmt, ...);
// need to free this
API string readFile(const char *path);
API uint32_t hashString(const string str);
API uint32_t hashBits(const uint64_t hash);

API void svSplitCharToBuf(stringViews *buf, const stringView s,
                          const char delim);
API stringViews svSplitChar(const stringView s, const char delim);
API stringView svTrimRight(stringView s);
API stringView svTrimLeft(stringView s);
API stringView svTrim(stringView s);

// the actual implementaion
// #define COMMON_IMPLEMENTATION
#ifdef COMMON_IMPLEMENTATION

API uint32_t hashString(string str) {
    uint32_t hash = 2166136261u;
    for (size_t i = 0; i < str.cnt; i++) {
        hash ^= (uint8_t)str.items[i];
        hash *= 16777619;
    }
    return hash;
}

API bool stringEmpty(string s) { return s.items == NULL && s.cnt == 0; }

// hot path for when the lengths are different
// and if they both point to the same location
API bool stringsEqual(string a, string b) {
    if (a.cnt != b.cnt) return false;
    if (a.items == b.items) return true;
    return memcmp(a.items, b.items, a.cnt) == 0;
}

// returns a copy of the string
API string stringCopy(const string s) {
    char *buf = (char *)malloc(s.cnt + 1);
    strncpy(buf, s.items, s.cnt + 1);
    return (string){buf, s.cnt};
}

// this needs to be freed
// cstr has to be null terminated
// adds null terminator
API string stringFromCstr(const char *cstr) {
    size_t len = strlen(cstr);
    char *buf = (char *)malloc(len + 1);
    strncpy(buf, cstr, len + 1);
    return (string){buf, len};
}

// needs to be freed
// adds null terminator
API string stringFromFormat(const char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    size_t n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    char *buf = (char *)malloc(n + 1);
    va_start(args, fmt);
    vsnprintf(buf, n + 1, fmt, args);
    va_end(args);

    return (string){buf, n};
}

API int sbAppendf(stringBuilder *sb, const char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    // NOTE: the new_capacity needs to be +1 because of the null terminator.
    // However, further below we increase sb->count by n, not n + 1.
    // This is because we don't want the sb to include the null terminator. The
    // user can always sb_append_null() if they want it
    if (sb->cnt + n + 1 > sb->cap) {
        sb->cap = sb->cap < DA_INIT_CAP ? DA_INIT_CAP : sb->cap * 2;

        while ((sb->cnt + n + 1) > sb->cap) {
            sb->cap *= 2;
        }

        sb->items = (char *)DA_REALLOC(sb->items, sb->cap * sizeof(*sb->items));
        if (sb->items == NULL) {
            printf("oh no not enough memory");
            exit(1);
        }
    }

    char *dest = sb->items + sb->cnt;
    va_start(args, fmt);
    vsnprintf(dest, n + 1, fmt, args);
    va_end(args);

    sb->cnt += n;

    return n;
}

API void svSplitCharToBuf(stringViews *buf, const stringView s,
                          const char delim) {
    stringView sv = (stringView)s;
    while (sv.cnt != 0) {
        size_t i = 0;
        while (i < sv.cnt && sv.items[i] != delim) {
            i++;
        }

        if (buf->cnt + 1 > buf->cap) {
            if (buf->cap < 16) buf->cap = 16;
            while (buf->cnt + 1 > buf->cap) {
                buf->cap *= 2;
            }
            buf->items = (stringView *)DA_REALLOC(
                buf->items, buf->cap * sizeof(*buf->items));
            assert(buf->items != NULL && "Buy more RAM lol");
        }

        buf->items[buf->cnt++] = newSv(sv.items, i);

        if (i < sv.cnt) i++;
        sv.cnt -= i;
        sv.items += i;
    }
}

API stringViews svSplitChar(const stringView s, const char delim) {
    stringViews svs = {NULL, 0, 0};
    svSplitCharToBuf(&svs, s, delim);
    return svs;
}

API stringView svTrimRight(stringView s) {
    size_t i = 0;
    while (i < s.cnt && isspace(s.items[s.cnt - 1 - i])) {
        i++;
    }
    return newSv(s.items, s.cnt - i);
}

API stringView svTrimLeft(stringView s) {
    size_t i = 0;
    while (i < s.cnt && isspace(s.items[i])) {
        i++;
    }
    return newSv(s.items + i, s.cnt - i);
}

API stringView svTrim(stringView s) { return svTrimLeft(svTrimRight(s)); }

// need to free this
API string readFile(const char *path) {
    FILE *f = fopen(path, "r");
    if (f == NULL) {
        fprintf(stderr, "could not open '%s'\n", path);
        exit(74);
    }

    fseek(f, 0L, SEEK_END);
    size_t fsize = ftell(f);
    rewind(f);

    char *buffer = (char *)malloc(fsize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "buy more RAM lol\n");
        exit(74);
    }
    size_t nread = fread(buffer, sizeof(char), fsize, f);
    if (nread < fsize) {
        fprintf(stderr, "Could not read file '%s'\n", path);
        exit(74);
    }
    buffer[nread] = '\0';

    fclose(f);
    return (string){buffer, nread};
}

// From v8's ComputeLongHash() which in turn cites:
// Thomas Wang, Integer Hash Functions.
// http://www.concentric.net/~Ttwang/tech/inthash.htm
API uint32_t hashBits(uint64_t hash) {
    hash = ~hash + (hash << 18); // hash = (hash << 18) - hash - 1;
    hash = hash ^ (hash >> 31);
    hash = hash * 21; // hash = (hash + (hash << 2)) + (hash << 4);
    hash = hash ^ (hash >> 11);
    hash = hash + (hash << 6);
    hash = hash ^ (hash >> 22);
    return (uint32_t)(hash & 0x3fffffff);
}

#endif // COMMON_IMPLEMENTATION
#endif // INCLUDE_MYLIB_MYLIB_H_

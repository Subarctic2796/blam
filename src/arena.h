// Copyright 2022 Alexey Kutepov <reximkut@gmail.com>

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef ARENA_H_
#define ARENA_H_

#include <stddef.h>
#include <stdint.h>

#ifndef ARENA_NOSTDIO
#include <stdarg.h>
#include <stdio.h>
#endif // ARENA_NOSTDIO

#ifndef ARENA_ASSERT
#include <assert.h>
#define ARENA_ASSERT assert
#endif

#define ARENA_BACKEND_LIBC_MALLOC 0
#define ARENA_BACKEND_LINUX_MMAP  1

#ifndef ARENA_BACKEND
#define ARENA_BACKEND ARENA_BACKEND_LIBC_MALLOC
#endif // ARENA_BACKEND

typedef struct Region Region;

struct Region {
    Region *next;
    size_t cnt;
    size_t cap;
    uintptr_t data[];
};

typedef struct {
    Region *begin, *end;
} Arena;

typedef struct {
    Region *region;
    size_t cnt;
} Arena_Mark;

#ifndef ARENA_REGION_DEFAULT_CAP
#define ARENA_REGION_DEFAULT_CAP (8 * 1024)
#endif // ARENA_REGION_DEFAULT_CAP

Region *new_region(size_t cap);
void free_region(Region *r);

void *arena_alloc(Arena *a, size_t size_bytes);
void *arena_realloc(Arena *a, void *oldptr, size_t oldsz, size_t newsz);
char *arena_strdup(Arena *a, const char *cstr);
void *arena_memdup(Arena *a, void *data, size_t size);
void *arena_memcpy(void *dest, const void *src, size_t n);
#ifndef ARENA_NOSTDIO
char *arena_sprintf(Arena *a, const char *format, ...);
char *arena_vsprintf(Arena *a, const char *format, va_list args);
#endif // ARENA_NOSTDIO

Arena_Mark arena_snapshot(Arena *a);
void arena_reset(Arena *a);
void arena_rewind(Arena *a, Arena_Mark m);
void arena_free(Arena *a);
void arena_trim(Arena *a);

#ifndef ARENA_DA_INIT_CAP
#define ARENA_DA_INIT_CAP 256
#endif // ARENA_DA_INIT_CAP

#ifdef __cplusplus
#define cast_ptr(ptr) (decltype(ptr))
#else
#define cast_ptr(...)
#endif

#define arena_da_reserve(a, da, expected_cap)                                  \
    do {                                                                       \
        size_t new_cap = (da)->cap;                                            \
        if ((expected_cap) > new_cap) {                                        \
            if (new_cap < ARENA_DA_INIT_CAP) new_cap = ARENA_DA_INIT_CAP;      \
            while ((expected_cap) > new_cap) {                                 \
                new_cap *= 2;                                                  \
            }                                                                  \
            (da)->items = cast_ptr((da)->items) arena_realloc(                 \
                (a), (da)->items, (da)->cap * sizeof(*(da)->items),            \
                new_cap * sizeof(*(da)->items));                               \
            (da)->cap = new_cap;                                               \
            assert((da)->items != NULL && "Buy more RAM lol");                 \
        }                                                                      \
    } while (0)

#define arena_da_append(a, da, item)                                           \
    do {                                                                       \
        arena_da_reserve((a), (da), (da)->cnt + 1);                            \
        (da)->items[(da)->cnt++] = (item);                                     \
    } while (0)

// Append several items to a dynamic array
#define arena_da_append_many(a, da, new_items, new_items_cnt)                  \
    do {                                                                       \
        arena_da_reserve((a), (da), (da)->cnt + (new_items_cnt));              \
        arena_memcpy((da)->items + (da)->cnt, (new_items),                     \
                     (new_items_cnt) * sizeof(*(da)->items));                  \
        (da)->cnt += (new_items_cnt);                                          \
    } while (0)

// Append a sized buffer to a string builder
#define arena_sb_append_buf arena_da_append_many

// Append a NULL-terminated string to a string builder
#define arena_sb_append_cstr(a, sb, cstr)                                      \
    do {                                                                       \
        const char *s = (cstr);                                                \
        size_t n = arena_strlen(s);                                            \
        arena_da_append_many(a, sb, s, n);                                     \
    } while (0)

// Append a single NULL character at the end of a string builder. So then you
// can use it a NULL-terminated C string
#define arena_sb_append_null(a, sb) arena_da_append(a, sb, 0)

#endif // ARENA_H_

// #define ARENA_IMPLEMENTATION
#ifdef ARENA_IMPLEMENTATION

#if ARENA_BACKEND == ARENA_BACKEND_LIBC_MALLOC
#include <stdlib.h>

// TODO: instead of accepting specific capacity new_region() should accept
// the size of the object we want to fit into the region It should be up to
// new_region() to decide the actual capacity to allocate
Region *new_region(size_t cap) {
    size_t size_bytes = sizeof(Region) + sizeof(uintptr_t) * cap;
    // TODO: it would be nice if we could guarantee that the regions are
    // allocated by ARENA_BACKEND_LIBC_MALLOC are page aligned
    Region *r = (Region *)malloc(size_bytes);
    ARENA_ASSERT(r); // TODO: since ARENA_ASSERT is disableable go through
                     // all the places where we use it to check for failed
                     // memory allocation and return with NULL there.
    r->next = NULL;
    r->cnt = 0;
    r->cap = cap;
    return r;
}

void free_region(Region *r) { free(r); }
#elif ARENA_BACKEND == ARENA_BACKEND_LINUX_MMAP
#include <sys/mman.h>
#include <unistd.h>

Region *new_region(size_t cap) {
    size_t size_bytes = sizeof(Region) + sizeof(uintptr_t) * cap;
    Region *r = (Region *)mmap(NULL, size_bytes, PROT_READ | PROT_WRITE,
                               MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    ARENA_ASSERT(r != MAP_FAILED);
    r->next = NULL;
    r->cnt = 0;
    r->cap = cap;
    return r;
}

void free_region(Region *r) {
    size_t size_bytes = sizeof(Region) + sizeof(uintptr_t) * r->cap;
    int ret = munmap(r, size_bytes);
    ARENA_ASSERT(ret == 0);
}

#else
#error "Unknown Arena backend"
#endif

// TODO: add debug statistic collection mode for arena
// Should collect things like:
// - How many times new_region was called
// - How many times existing region was skipped
// - How many times allocation exceeded ARENA_REGION_DEFAULT_CAP

void *arena_alloc(Arena *a, size_t size_bytes) {
    size_t size = (size_bytes + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);

    if (a->end == NULL) {
        ARENA_ASSERT(a->begin == NULL);
        size_t cap = ARENA_REGION_DEFAULT_CAP;
        if (cap < size) cap = size;
        a->end = new_region(cap);
        a->begin = a->end;
    }

    while (a->end->cnt + size > a->end->cap && a->end->next != NULL) {
        a->end = a->end->next;
    }

    if (a->end->cnt + size > a->end->cap) {
        ARENA_ASSERT(a->end->next == NULL);
        size_t cap = ARENA_REGION_DEFAULT_CAP;
        if (cap < size) cap = size;
        a->end->next = new_region(cap);
        a->end = a->end->next;
    }

    void *result = &a->end->data[a->end->cnt];
    a->end->cnt += size;
    return result;
}

void *arena_realloc(Arena *a, void *oldptr, size_t oldsz, size_t newsz) {
    if (newsz <= oldsz) return oldptr;
    void *newptr = arena_alloc(a, newsz);
    char *newptr_char = (char *)newptr;
    char *oldptr_char = (char *)oldptr;
    for (size_t i = 0; i < oldsz; ++i) {
        newptr_char[i] = oldptr_char[i];
    }
    return newptr;
}

size_t arena_strlen(const char *s) {
    size_t n = 0;
    while (*s++) {
        n++;
    }
    return n;
}

void *arena_memcpy(void *dest, const void *src, size_t n) {
    char *d = (char *)dest;
    const char *s = (const char *)src;
    for (; n; n--) {
        *d++ = *s++;
    }
    return dest;
}

char *arena_strdup(Arena *a, const char *cstr) {
    size_t n = arena_strlen(cstr);
    char *dup = (char *)arena_alloc(a, n + 1);
    arena_memcpy(dup, cstr, n);
    dup[n] = '\0';
    return dup;
}

void *arena_memdup(Arena *a, void *data, size_t size) {
    return arena_memcpy(arena_alloc(a, size), data, size);
}

#ifndef ARENA_NOSTDIO
char *arena_vsprintf(Arena *a, const char *format, va_list args) {
    va_list args_copy;
    va_copy(args_copy, args);
    int n = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);

    ARENA_ASSERT(n >= 0);
    char *result = (char *)arena_alloc(a, n + 1);
    vsnprintf(result, n + 1, format, args);

    return result;
}

char *arena_sprintf(Arena *a, const char *format, ...) {
    va_list args;
    va_start(args, format);
    char *result = arena_vsprintf(a, format, args);
    va_end(args);

    return result;
}
#endif // ARENA_NOSTDIO

Arena_Mark arena_snapshot(Arena *a) {
    if (a->end == NULL) { // snapshot of uninitialized arena
        ARENA_ASSERT(a->begin == NULL);
        return (Arena_Mark){a->end, 0};
    }
    return (Arena_Mark){a->end, a->end->cnt};
}

void arena_reset(Arena *a) {
    for (Region *r = a->begin; r != NULL; r = r->next) {
        r->cnt = 0;
    }

    a->end = a->begin;
}

void arena_rewind(Arena *a, Arena_Mark m) {
    if (m.region == NULL) { // snapshot of uninitialized arena
        arena_reset(a);     // leave allocation
        return;
    }

    m.region->cnt = m.cnt;
    for (Region *r = m.region->next; r != NULL; r = r->next) {
        r->cnt = 0;
    }

    a->end = m.region;
}

void arena_free(Arena *a) {
    Region *r = a->begin;
    while (r) {
        Region *r0 = r;
        r = r->next;
        free_region(r0);
    }
    a->begin = NULL;
    a->end = NULL;
}

void arena_trim(Arena *a) {
    Region *r = a->end->next;
    while (r) {
        Region *r0 = r;
        r = r->next;
        free_region(r0);
    }
    a->end->next = NULL;
}

#endif // ARENA_IMPLEMENTATION

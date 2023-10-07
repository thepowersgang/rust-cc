#pragma once

#include "stddef.h"

extern void abort(void);

extern int atoi(const char *nptr);
extern long atol(const char *nptr);
extern long long atoll(const char *nptr);

extern long strtol(const char *nptr, char **endptr, int base);
extern long long strtoll(const char *nptr, char **endptr, int base);

extern void *malloc(size_t size);
extern void free(void *ptr);
extern void *calloc(size_t nmemb, size_t size);
extern void *realloc(void *ptr, size_t size);

extern void exit(int status) __attribute__((noreturn));


static inline int abs(int j) { if(j < 0) { return -j; } else { return j; } }
static inline long labs(long j) { if(j < 0) { return -j; } else { return j; } }
static inline long long llabs(long long j) { if(j < 0) { return -j; } else { return j; } }

#define __builtin_return_address(x)	0


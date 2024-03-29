#pragma once

extern int strcmp(const char *s1, const char *s2);
extern int strncmp(const char *s1, const char *s2, size_t n);
extern size_t strlen(const char *s);

extern char *strcpy(char *dest, const char *src);
extern char *strncpy(char *dest, const char *src, size_t n);

extern void *memcpy(void *dest, const void *src, size_t n);
extern void *memset(void *s, int c, size_t n);
extern void *memmove(void *dest, const void *src, size_t n);
extern int memcmp(const void *s1, const void *s2, size_t n);

extern char *strdup(const char *s);
// POSIX >=200809
extern char *strndup(const char *s, size_t n);

extern char *strchr(const char *s, int c);
extern char *strrchr(const char *s, int c);

extern char *strerror(int errnum);



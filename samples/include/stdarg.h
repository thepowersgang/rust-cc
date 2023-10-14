#pragma once

typedef __magictype__("va_list:void")	va_list;

#define va_start(__list, __first)	__magiccall__("va_start" : __list, __first :)
#define va_end(__list)	__magiccall__("va_end" : __list :)
#define va_arg(__list, __ty)	__magiccall__("va_arg" : __list : __ty)
#define va_copy(__dst, __src)	__magiccall__("va_copy" : __dst, __src : )

#include <stdio.h>

extern int vprintf(const char *format, va_list ap);
extern int vfprintf(FILE *stream, const char *format, va_list ap);
extern int vdprintf(int fd, const char *format, va_list ap);
extern int vsprintf(char *str, const char *format, va_list ap);
extern int vsnprintf(char *str, size_t size, const char *format, va_list ap);


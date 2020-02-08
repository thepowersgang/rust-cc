#pragma once

typedef __magictype__("va_list:*void")	va_list;

#define va_arg(__list, __ty)	__magiccall__("va_arg" : __list : __ty)

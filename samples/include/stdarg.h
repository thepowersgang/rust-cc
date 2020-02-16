#pragma once

typedef __magictype__("va_list:*void")	va_list;

#define va_start(__list, __first)	__magiccall__("va_start" : __list, __first :)
#define va_end(__list)	__magiccall__("va_end" : __list :)
#define va_arg(__list, __ty)	__magiccall__("va_arg" : __list : __ty)

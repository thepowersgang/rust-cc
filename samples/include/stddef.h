#pragma once

#ifdef __RCC__
typedef __magictype__("size_t:uptr")	size_t;
typedef __magictype__("ssize_t:iptr")	ssize_t;
typedef __magictype__("wchar_t:u32")	wchar_t;
#else
typedef unsigned long	size_t;
typedef unsigned long	ssize_t;
typedef unsigned long	wchar_t;
#endif

#define NULL	((void*)0)

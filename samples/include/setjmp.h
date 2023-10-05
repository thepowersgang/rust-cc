#pragma once

typedef __magictype__("jmp_buf:v64")	jmp_buf;

extern int setjmp(jmp_buf env);
extern void longjmp(jmp_buf env, int val);
// POSIX 2001
//extern int sigsetjmp(sigjmp_buf env, int savesigs);
//extern void siglongjmp(sigjmp_buf env, int val);

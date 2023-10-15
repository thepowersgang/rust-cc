#pragma once

typedef long    regoff_t;

typedef struct _regex_s {
    size_t  re_nsub; /* Number of parenthesized subexpressions. */
} regex_t;
typedef struct _regmatch_s {
    regoff_t    rm_so; /* Byte offset from start of string
                        to start of substring */
    regoff_t    rm_eo; /* Byte offset from start of string of
                        the first character after the end of
                        substring */
} regmatch_t;

#define	REG_EXTENDED	1
#define REG_ICASE	2
#define REG_NOSUB	4
#define	REG_NEWLINE	8

extern int regcomp(regex_t *preg, const char *regex, int cflags);
extern int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
extern void regfree(regex_t *preg);

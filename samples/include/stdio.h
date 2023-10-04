#pragma once

typedef struct FILE	FILE;
typedef unsigned long long	fpos_t;

extern FILE*	stdin;
extern FILE*	stdout;
extern FILE*	stderr;

extern void perror(const char* s);

extern int printf(const char *format, ...);
extern int fprintf(FILE *stream, const char *format, ...);
extern int dprintf(int fd, const char *format, ...);
extern int sprintf(char *str, const char *format, ...);
extern int snprintf(char *str, size_t size, const char *format, ...);

extern int scanf(const char *format, ...);
extern int fscanf(FILE *stream, const char *format, ...);
extern int sscanf(const char *str, const char *format, ...);

extern FILE *fopen(const char *pathname, const char *mode);
//extern FILE *fdopen(int fd, const char *mode);
extern FILE *freopen(const char *pathname, const char *mode, FILE *stream);

enum {
	SEEK_END = -1,
	SEEK_CUR = 0,
	SEEK_SET = 1,
};
extern int fseek(FILE *stream, long offset, int whence);
extern long ftell(FILE *stream);
extern void rewind(FILE *stream);
extern int fgetpos(FILE *stream, fpos_t *pos);
extern int fsetpos(FILE *stream, const fpos_t *pos);

extern size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

extern int fflush(FILE *stream);
extern int fclose(FILE *stream);

extern int fgetc(FILE *stream);
extern char *fgets(char *s, int size, FILE *stream);
extern int getc(FILE *stream);
extern int getchar(void);
extern int ungetc(int c, FILE *stream);

extern int fputc(int c, FILE *stream);
extern int fputs(const char *s, FILE *stream);
extern int putc(int c, FILE *stream);
extern int putchar(int c);
extern int puts(const char *s);


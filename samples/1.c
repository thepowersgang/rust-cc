// A helper that stress tests the compiler

//#include <stdio.h>
#define/**/ foo(bar) baz
#define baz
foo(_)
baz

//extern int printf(const char*, ...);
extern int printf(const char*, const char*);
int main(int argc, const char* argv[]) {
	//printf("Hello, %s! float %f", "world", 1.23f);
	printf("Hello, %s!", "world");

	for(int i = 0; i < 10; i ++)
	{
		(void)0;
	}

	return 0;
}


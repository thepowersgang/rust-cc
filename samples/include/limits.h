#pragma once

#define SIZE_MAX	0xFFFFFFFFul
#define UINT_MAX	(~0u)
#define ULONG_MAX	(~0ul)
#define ULLONG_MAX	(~0ull)
#define LLONG_MAX	((long long)(~0ull >> 1))
#define LLONG_MIN	(-LLONG_MAX - 1)
#define LONG_MAX	(long)(~0ul >> 1)
#define LONG_MIN	((-LONG_MAX - 1))
#define INT_MAX 	((int)(~0u >> 1))
#define INT_MIN 	(-INT_MAX - 1)

#pragma once

typedef void (*sighandler_t)(int);

extern sighandler_t signal(int signum, sighandler_t handler);

enum {
	SIGINT = 2,
};


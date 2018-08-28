#include <stdio.h>

#define foo(x) x + 3

int
main(int argc, char *argv[])
{
	printf("foo(5) - 2 = %d\n", foo(5) - 2);
	printf("foo(5) * 2 = %d\n", foo(5) * 2);
	return 0;
}

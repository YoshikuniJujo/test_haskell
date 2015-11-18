#include <stdio.h>

static void
function(int a)
{
	printf("my_function: %d\n", a);
}

static void
caller(void (*new_function)(int a), int p)
{
	(*new_function)(p);
}

int
main(void)
{
	caller(function, 10);
	return 0;
}

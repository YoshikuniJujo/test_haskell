#include <stdio.h>

static int
my_function(int a)
{
	printf("my_function: %d\n", a);
	return 2 * a + 3;
}

int
main(void)
{
	int (*new_function)(int a) = my_function;
	int x;

	x = (*new_function)(10);
	printf("main: %d\n", x);
	return 0;
}

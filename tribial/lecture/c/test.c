#include <stdio.h>

int
add(int x, int y) {
	return x + y;
}

int
main(int argc, char *argv[])
{
	printf("%d\n", add(3, 4));

	return 0;
}

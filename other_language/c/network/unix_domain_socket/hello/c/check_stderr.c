#include <stdio.h>

int
main (int argc, char argv[])
{
	printf("stderr: %p\n", stderr);
	printf("sizeof(stderr): %ld\n", sizeof(stderr));
	return 0;
}

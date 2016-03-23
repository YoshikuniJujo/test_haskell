#include <stdio.h>

int i;
int j;

int
main(int argc, char *argv[])
{
	i = 10;

	asm (
		"movl i,%eax	\n\t"
		"add $10, %eax	\n\t"
		"movl %eax, j");
	printf("%d %d\n", i, j);

	return 0;
}

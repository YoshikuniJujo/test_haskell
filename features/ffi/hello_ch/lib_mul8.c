#include <stdio.h>
#include "mul8.h"

int
main(int argc, char *argv[])
{
	mul8_init();

	printf("%d\n", mul8(6));

	mul8_end();
	return 0;
}

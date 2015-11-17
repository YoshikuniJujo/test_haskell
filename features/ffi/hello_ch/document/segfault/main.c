#include "segfault.h"

int
main(int argc, char *argv[])
{
	init(&argc, &argv);

	end();
	return 0;
}

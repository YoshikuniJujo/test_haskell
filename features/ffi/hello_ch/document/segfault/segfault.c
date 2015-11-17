#include "HsFFI.h"

int
init(int *argc, char **argv[])
{
	hs_init(argc, argv);
	return 0;
}

void
end(void)
{
	hs_exit();
}

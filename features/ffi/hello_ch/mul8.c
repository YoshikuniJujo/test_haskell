#include <stdlib.h>
#include "HsFFI.h"

HsBool
mul8_init(void)
{
	int argc = 0;
	char *argv[] = { NULL };
	char **pargv = argv;

	hs_init(&argc, &pargv);

	return HS_BOOL_TRUE;
}

void
mul8_end(void)
{
	hs_exit();
}

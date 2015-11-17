#include <stdio.h>
#include <stdlib.h>
#include "HsFFI.h"
#include "Foo_stub.h"

HsBool
mylib_init(void)
{
	int argc = 0;
//	char *argv[] = { "+RTS", "-A32m", NULL };
	char *argv[] = { NULL };
	char **pargv = argv;

	hs_init(&argc, &pargv);

	return HS_BOOL_TRUE;
}

void
mylib_end(void)
{
	hs_exit();
}

int
run(void)
{
	int i;

//	for (i = 0; i < 5; i++) {
		printf("%d\n", foo(2500));
//	}

	return 0;
}

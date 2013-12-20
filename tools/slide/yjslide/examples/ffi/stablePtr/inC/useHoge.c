#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Hoge_stub.h"
#endif

int
main(int argc, char *argv[])
{
	int i;
	HsStablePtr t;

	hs_init(&argc, &argv);

	t = tarou();
	for (i = 0; i < 10; i++)
		print_tarou(t);

	hs_exit();

	return 0;
}

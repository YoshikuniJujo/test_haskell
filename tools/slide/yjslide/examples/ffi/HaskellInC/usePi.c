#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Pi_stub.h"
#endif

int
main(int argc, char *argv[])
{
	hs_init(&argc, &argv);

	printf("Hello, world!\n");
	printf("%1.10f\n", h_pi(10000000));

	hs_exit();

	return 0;
}

#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Mul8_stub.h"
#endif

int
main(int argc, char *argv[])
{
	hs_init(&argc, &argv);

	printf("%d\n", mul8(6));

	hs_exit();
	return 0;
}

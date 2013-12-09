#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Foo_stub.h"
#endif

int
main(int argc, char *argv[])
{
	hs_init(&argc, &argv);

	printf("Hello, world!\n");
	printf("%d\n", foo(2500));

	hs_exit();
	return 0;
}

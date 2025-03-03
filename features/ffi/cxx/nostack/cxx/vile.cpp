#include <stdio.h>
#include "prototypes.h"

void
c_routine (void)
{
	printf("Hello, from c_routine!\n");
	haskell_definition();
}

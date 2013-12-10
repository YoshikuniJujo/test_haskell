#include <stdio.h>
#include "name.h"

int
main(int argc, char *argv)
{
	name *n = mkName("Yoshikuni", "Jujo");
	printName(n);
	freeName(n);

	return 0;
}

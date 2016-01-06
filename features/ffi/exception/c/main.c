#include <stdio.h>
#include "script.h"

int
main(int argc, char *argv[])
{
	int ret;

	ret = run_script("alreadyexist.script");

	if (ret != 0) {
		printf("ERROR: error code (%d)\n", ret);
	}

	return 0;
}

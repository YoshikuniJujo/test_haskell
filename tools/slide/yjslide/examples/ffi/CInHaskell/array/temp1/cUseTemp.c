#include <stdio.h>
#include "temp.h"

int main(int argc, char *argv[]) {

	int i, *ts = get_temps();
	for (i = 0; i < 12; i++) {
		printf("%2d: %d\n", i + 1, ts[i]);
	}

	return 0;
}

#include <stdio.h>
#include "names.h"

int main(int argc, char *argv[]) {
	char **n = get_names();
	int i, len = get_size();

	for (i = 0; i < len; i++) {
		printf("%s\n", *n++);
	}

	return 0;
}

#include <stdlib.h>
#include "names.h"

char *names[] = {
	"Tarou",
	"Jirou",
	"Saburou",
	NULL
};

int get_size(void) {
	char **n = names;
	int i = 0;
	while (*n != NULL) { n++; i++; }
	return i;
}

char **get_names(void) { return names; }

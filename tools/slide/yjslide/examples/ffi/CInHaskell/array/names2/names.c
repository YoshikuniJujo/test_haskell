#include <stdlib.h>
#include "names.h"

char *names[] = {
	"Tarou",
	"Jirou",
	"Saburou",
	NULL
};

char **get_names(void) {
	return names;
}

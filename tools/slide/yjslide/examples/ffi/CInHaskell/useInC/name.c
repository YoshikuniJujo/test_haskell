#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "name.h"

name*
mkName(char *first, char *last) {
	name *n = calloc(1, sizeof(name));

//	strcpy(n->first_name, first);
//	strcpy(n->last_name, last);

	strncpy(n->first_name, first, 19);
	strncpy(n->last_name, last, 19);

	return n;
}

void
printName(name* n) {
	printf("%s %s\n", n->first_name, n->last_name);
}

void
freeName(name* n) {
	free(n);
}

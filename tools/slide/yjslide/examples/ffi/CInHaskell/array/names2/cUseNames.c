#include <stdio.h>
#include "names.h"

int main(int argc, char *argv[]) {
	char **ns = get_names();

	for(;;) {
		if (*ns == NULL) break;
		printf("%s\n", *ns);
		ns++;
	}
	
	return 0;
}

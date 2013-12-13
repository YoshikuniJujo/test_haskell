#include <stdio.h>
#include "apply_123.h"

void print_apply_123(int (*f)(int)) {
	int i;
	for (i = 0; i < 3; i++) {
		printf("%d\n", f(i + 1));
	}
}

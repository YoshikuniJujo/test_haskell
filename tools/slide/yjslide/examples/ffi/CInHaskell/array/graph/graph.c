#include <stdio.h>
#include "graph.h"

void mk_graph(int *dat, int n) {
	int i, j;
	for (i = 0; i < n; i++) {
		for (j = 0; j < dat[i]; j++) printf("*");
		printf("\n");
	}
}

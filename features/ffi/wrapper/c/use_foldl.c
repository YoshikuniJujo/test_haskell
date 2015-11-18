#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "foldl.h"

int read1(char s[]) { return s[0] - '0'; }

int
main(int argc, char *argv[])
{
	char c = '\0'; Op op = NULL; int x0 = 0;
	int n = argc - 2; int xs[100];
	int i = 0;

	if (n < 0) exit(1);
	if (!strcmp(argv[1], "sum")) { x0 = 0; c = '+'; op = add; }
	if (!strcmp(argv[1], "product")) { x0 = 1; c = '*'; op = mul; }
	if (op == NULL) exit(1);

	for (i = 0; i < 100; i++) {
		xs[i] = read1(argv[i + 2]);
		printf("%d ", xs[i]);
		if (i == n - 1) break;
		printf("%c ", c); }
	printf("= %d\n", foldl(op, x0, n, xs));

	return 0;
}

#include <stdio.h>
#include <stdlib.h>

typedef int (*Op)(int x, int y);

int ad(int x, int y) { return x + y; }
int sb(int x, int y) { return x - y; }
int ml(int x, int y) { return x * y; }
int dv(int x, int y) { return x / y; }

int
main(int argc, char *argv[])
{
	Op op = NULL;
	char c = '\0';
	int x = 0;
	int y = 0;

	if (argc != 4) exit(1);

	c = argv[2][0];
	x = argv[1][0] - '0';
	y = argv[3][0] - '0';

	switch (c) {
		case '+': op = ad; break;
		case '-': op = sb; break;
		case 'x': op = ml; break;
		case '/': op = dv; break; }

	if (op == NULL) printf("No such operator: %c\n", c);
	else printf("%d %c %d = %d\n", x, c, y, op(x, y));

	return 0;
}

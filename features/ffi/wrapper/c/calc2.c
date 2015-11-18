#include <stdio.h>
#include <stdlib.h>

typedef int (*Op)(int x, int y);

int ad(int x, int y) { return x + y; }
int sb(int x, int y) { return x - y; }
int ml(int x, int y) { return x * y; }
int dv(int x, int y) { return x / y; }

Op
operator(char c)
{
	switch (c) {
		case '+': return ad;
		case '-': return sb;
		case 'x': return ml;
		case '/': return dv; }
	return NULL;
}

int
main(int argc, char *argv[])
{
	Op op = NULL;
	char c = '\0';
	int x = 0;
	int y = 0;

	if (argc != 4) exit(1);

	c = argv[2][0];
	op = operator(c);
	x = argv[1][0] - '0';
	y = argv[3][0] - '0';

	if (op == NULL) printf("No such operator: %c\n", c);
	else printf("%d %c %d = %d\n", x, c, y, op(x, y));

	return 0;
}

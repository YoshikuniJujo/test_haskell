#include <stdio.h>

typedef int (*Compare)(int x, int y);

int
compare(int x, int y)
{
	if (x < y) return - 1;
	else if (x == y) return 0;
	else return 1;
}

int
compare2(int x, int y)
{
	if (x > y) return -1;
	else if (x == y) return 0;
	else return 1;
}

int select(Compare cmp, int x, int y)
{
	if (cmp(x, y) >= 0) return x; return y;
}

int
main(int argc, char *argv[])
{
	printf("%d\n", select(compare, 3, 8));
	printf("%d\n", select(compare2, 3, 8));

	return 0;
}

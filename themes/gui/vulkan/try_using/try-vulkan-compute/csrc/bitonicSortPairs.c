#include <stdio.h>

void
try_separate_num(int n)
{
	int i;

	for (i = 0; i < n; i++) n = n - i - 1;

	printf("i = %d, n = %d\n", i, n);
}

int
bit_separate(int bs, int i)
{
	int u, l;

	u = bs >> i << i;
	l = bs ^ u;

	return u << 1 | l;
}

int
check_bit(int bs, int i)
{
	return bs >> i & 1;
}

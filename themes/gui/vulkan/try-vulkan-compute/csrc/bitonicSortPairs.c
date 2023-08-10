#include <stdio.h>

void try_separate_num (int n)
{

	int i;

	for (i = 0; i < n; i++) n = n - i - 1;

	printf("i = %d, n = %d\n", i, n);

}

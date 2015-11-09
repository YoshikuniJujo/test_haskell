#include <stdio.h>
#include "proc_array.h"

int
main(int argc, char argv[])
{
	int ns[31];
	int i = 0;

	proc_array(10, 30, ns);
	for (i = 0; i < 100; i++) {
		if (ns[i] == -1) break;
		printf("%d ", ns[i]); }
	printf("\n");

	proc_array(30, 30, ns);
	for (i = 0; i < 100; i++) {
		if (ns[i] == -1) break;
		printf("%d ", ns[i]); }
	printf("\n");


	return 0;
}

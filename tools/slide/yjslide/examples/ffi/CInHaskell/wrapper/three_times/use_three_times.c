#include <stdio.h>
#include "three_times.h"

void print_hello(void) {
	printf("Hello\n");
}

int main(int argc, char *argv[]) {
	
	three_times(print_hello);

	return 0;
}

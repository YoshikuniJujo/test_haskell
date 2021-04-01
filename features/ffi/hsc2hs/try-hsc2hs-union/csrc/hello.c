#include <stdio.h>
#include "hello.h"

void
hello(void)
{
	printf("Hello, world!\n");
}

foo sample_foo = { .integers = { 123, 456 } };

foo *
get_sample_foo (void)
{
	return &sample_foo;
}

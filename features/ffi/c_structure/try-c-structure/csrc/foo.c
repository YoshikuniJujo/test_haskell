#include <stdlib.h>
#include <stdio.h>
#include "foo.h"

Foo *
foo_copy(Foo *src)
{
	Foo *p = malloc(sizeof(Foo));
	p -> x = src -> x;
	p -> y = src -> y;
	return p;
}

void
foo_free(Foo *p)
{
	free(p);
}

void
foo_print(Foo *f)
{
	printf("Foo: x = %d, y = %d\n", f -> x, f -> y);
}

void
foo_scale(Foo *f, int s)
{
	f -> x = f -> x * s;
	f -> y = f -> y * s;
}

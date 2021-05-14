#include <stdlib.h>
#include "foo.h"

Foo sampleFoo = { 123, 456 };

Foo *sample_foo() { return &sampleFoo; }

Foo *
foo_copy(Foo *src)
{
	Foo *p = malloc(sizeof(Foo));
	p->x = src->x;
	p->y = src->y;
	return p;
}

void
foo_free(Foo *p)
{
	free(p);
}

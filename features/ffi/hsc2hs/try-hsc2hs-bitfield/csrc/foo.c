#include <stdlib.h>

#include "foo.h"

struct Bar *
sample_bar(void)
{
	struct Bar *p = (struct Bar *)malloc(sizeof(struct Bar));
	p->bee = 123;
	p->boo = 456;
	p->beigh = 789;
	p->bough = 999;
	return p;
}

struct Baz *
sample_baz(void)
{
	struct Baz *p = (struct Baz *)malloc(sizeof(struct Baz));
	p->b0 = 0;
	p->b1 = 1;
	p->b2_5 = 9;
	return p;
}

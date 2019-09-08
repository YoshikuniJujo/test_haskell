#include "hello.h"

int
add123(int n)
{
	return n + 123;
}

int eight = 8;

int
return_eight()
{
	return eight;
}

int
return_add123_(int n)
{
	return add123_(n);
}

int
return_foo_foo1(struct foo *f)
{
	return foo_foo1(f);
}

struct foo foo0 = { 456, 789 };

struct foo*
return_foo0()
{
	return &foo0;
}

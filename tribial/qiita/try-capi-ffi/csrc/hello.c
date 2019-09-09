#include "hello.h"

int
add123(int n)
{
	return n + 123;
}

int eight = 8;

struct point point0;

struct point*
make_point(int x, int y)
{
	point0.x = x;
	point0.y = y;
	return &point0;
}

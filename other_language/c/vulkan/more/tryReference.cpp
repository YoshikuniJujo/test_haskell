#include <stdio.h>

struct Foo { int bar; int baz; };

int
main(int argc, char *argv[])
{
	Foo foo = { 123, 456};

	int b = foo.bar;
	b = 789;
	printf("%d %d\n", foo.bar, foo.baz);
	
	int& c = foo.bar;
	c = 987;
	printf("%d %d\n", foo.bar, foo.baz);

	return 0;
}

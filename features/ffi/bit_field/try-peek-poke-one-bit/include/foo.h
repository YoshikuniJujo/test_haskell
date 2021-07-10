#ifndef _FOO_H
#define _FOO_H

typedef struct {
	int a;
	int b;
	unsigned int c : 1;
} Foo;

#define peek_foo_c(p) (p->c)

#define poke_foo_c(p, x) (p->c = x)

#endif

#include <stdlib.h>
#include <stdint.h>

typedef struct stack { int p; int q; struct stack *next; } _stack;

typedef struct stack** stack;

stack
empty(void)
{
	stack s;
	s = malloc(sizeof(stack));
	*s = NULL;
	return s;
}

void
push(int p, int q, stack s)
{
	struct stack *h;
	h = malloc(sizeof(struct stack));
	h->p = p; h->q = q;
	h->next = *s;
	*s = h;
}

int
pop(int *p, int *q, stack s)
{
	if (*s == NULL) return 0;

	*p = (*s)->p; *q = (*s)-> q; *s = (*s)->next;
	return 1;
}

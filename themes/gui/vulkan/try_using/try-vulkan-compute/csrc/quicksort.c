#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

struct stack { int p; int q; struct stack *next; };
typedef struct stack** stack;

stack
empty(void)
{
	stack s;
	s = malloc(sizeof(stack)); *s = NULL;
	return s;
}

int
is_empty(stack s)
{
	return ((*s) == NULL);
}

void
push(int p, int q, stack s)
{
	struct stack *h;
	h = malloc(sizeof(struct stack));
	h->p = p; h->q = q; h->next = *s; *s = h;
}

int
pop(int *p, int *q, stack s)
{
	if (*s == NULL) return 0;

	struct stack *h = *s;
	*p = h->p; *q = h-> q; *s = h->next; free(h);
	return 1;
}

void
simpleinsertsort(int n, uint32_t ks[])
{
	for (int j = 2; j < n + 1; j++) {
		if (ks[j - 1] > ks[j]) {
			uint32_t k = ks[j]; int i;
			for (i = j - 1; ks[i] > k; i--) ks[i + 1] = ks[i];
			ks[i + 1] = k;
		}
	}
}

void
quicksort(int m, int n, uint32_t ks[])
{
	if (n <= m) { simpleinsertsort(n, ks); return; }

	stack st = empty(); int l = 1, r = n;

	for(;;) {

		int i = l, j = r + 1, k = ks[l];

		for (;;) {
			for (i++; ks[i] < k; i++); for (j--; k < ks[j]; j--);
			if (j <= i) break;
			uint32_t t = ks[i]; ks[i] = ks[j]; ks[j] = t;
		}

		uint32_t t = ks[l]; ks[l] = ks[j]; ks[j] = t;

		if (r - j >= j - l && j - l >= m) {
			push(j + 1, r, st); r = j - 1;
		} else if (j - l >= r - j && r - j >= m) {
			push(l, j - 1, st); l = j + 1;
		} else if (r - j >= m && m >= j - l) {
			l = j + 1;
		} else if (j - l >= m && m >= r - j) {
			r = j - 1;
		} else {
			int *ll, *rr;
			ll = malloc(sizeof(int)); rr = malloc(sizeof(int));
			int cs = pop(ll, rr, st);
			if (cs) { l = *ll; r = *rr; } else break;
		}
	}

	simpleinsertsort(n, ks);
}

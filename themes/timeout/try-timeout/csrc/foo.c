#include <stdio.h>

char *
fgets_printf (char *b, int n, FILE* strm)
{
	char *r;
	r = fgets(b, n, strm);
	if (r != NULL) printf("%s", b);
	else printf("%p", r);
	return r;
}

#include "CVect.h"

#include <stdio.h>

void
printCVect(CVect *v)
{
	printf("CVect: %d %d `%s'\n", v->x, v->y, v->label);
}

void
add(CVect *a)
{
	a->x++;
	a->y++;
}

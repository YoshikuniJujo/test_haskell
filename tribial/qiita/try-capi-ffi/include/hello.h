#ifndef _HELLO_H
#define _HELLO_H

int add123(int n);

extern int eight;

#define ADD123(n) ((n)+123)

struct point {
	int x;
	int y; };

struct point* make_point(int x, int y);

#define POINT_X(p)	(p->x)

#endif

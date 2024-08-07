#include <stdint.h>

void
swap(uint32_t ns[], int i, int j) {
	if (ns[i] > ns[j]) { uint32_t t = ns[i]; ns[i] = ns[j]; ns[j] = t; }
}

void
bitonicsort(int n, uint32_t ns[])
{
	for (int p = 0; p < n; p++) for (int q = 0; q <= p; q++)
	for (int i = 0; i < (1 << (n - 1)); i++) {
		int r = p - q;
		int u = i >> r << r; int l = i ^ u;
		int x = u << 1 | l; int y = x | 1 << r;
		int f, t;
		if ((i >> p & 1)) { f = y; t = x; } else { f = x; t = y; }
		swap(ns, f, t);
	}
}

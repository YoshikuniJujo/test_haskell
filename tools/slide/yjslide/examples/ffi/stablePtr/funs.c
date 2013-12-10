#include "HsFuns_stub.h"

int fun1(int n) { return n + 8; }

void useHSFun() {
	hsfun();
}

HsStablePtr returnEight(void) { return eight(); }

void printEight(void) {
	HsStablePtr p = eight();
	int i;
	for (i = 0; i < 10; i++) printSP(p);
}

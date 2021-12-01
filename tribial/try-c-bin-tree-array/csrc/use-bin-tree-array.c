#include <stdio.h>
#include <bin-tree-array.h>
#include <use-bin-tree-array.h>

int tree[TREE_SIZE(5)] = {
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };

void
depth(int *tr, int i, int h)
{
	if (h <= 0) { return; };

	printf("%d\n", tr[i]);
	depth(tr, LEFT_CHILD(i), h - 1);
	depth(tr, RIGHT_CHILD(i), h - 1);
}

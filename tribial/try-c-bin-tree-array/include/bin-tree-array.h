#ifndef _BIN_TREE_ARRAY_H
#define _BIN_TREE_ARRAY_H

#define TREE_SIZE(h)	((1 << h) - 1)

#define LEFT_CHILD(i)	((i) * 2 + 1)
#define RIGHT_CHILD(i)	((i) * 2 + 2)
#define PARENT(i)	(((i) + 1) / 2 - 1)

#endif

#ifndef _MEM_ALLOC_LOCAL_H
#define _MEM_ALLOC_LOCAL_H

#include <stdbool.h>

#define TREE_SIZE(h)	((1 << h) - 1)

#define LEFT_CHILD(i)	((i) * 2 + 1)
#define RIGHT_CHILD(i)	((i) * 2 + 2)
#define PARENT(i)	(((i) + 1) / 2 - 1)

typedef struct { bool allocated; int used; } AllocInfo;
extern AllocInfo alloc_info[];

#endif

#ifndef _USE_BIN_TREE_ARRAY_H
#define _USE_BIN_TREE_ARRAY_H

#include <stdbool.h>
#include <stdint.h>
#include <bin-tree-array.h>

extern int tree[];

typedef struct {
	bool allocated;
	int used; } AllocInfo;

extern AllocInfo alloc_info[];

extern uint64_t memory[];

#endif

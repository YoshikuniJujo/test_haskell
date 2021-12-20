#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <mem_alloc_local.h>

AllocInfo alloc_info[TREE_SIZE(8)];
uint64_t memory[1 << 7];

int
get_index(int sz, int dp, int ii, int mi)
{
	AllocInfo *inf = &(alloc_info[ii]);
	AllocInfo *l = &(alloc_info[LEFT_CHILD(ii)]);
	AllocInfo *r = &(alloc_info[RIGHT_CHILD(ii)]);

	if (inf->used + sz > (1024 >> dp)) {
		while (ii) { ii = PARENT(ii); alloc_info[ii].used -= sz; }
		return -1; }

	inf->used += sz;
	if (sz << dp == 1024) { inf->allocated = true; return mi; }
	else if (l->used <= r->used) get_index(sz, dp + 1, LEFT_CHILD(ii), mi);
	else get_index(sz, dp + 1, RIGHT_CHILD(ii), mi + (64 >> dp));
}

int normalize(int sz)
{
	if (sz <= 8) return 8;

	int rslt = 1, tmp = sz - 1;
	while (tmp) { tmp >>= 1; rslt <<= 1; }
	return rslt;
}

void *
allocate_memory(int sz)
{
	int i = get_index(normalize(sz), 0, 0, 0);
	if (i < 0) return NULL; else return memory + i;
}

int
free_memory(void *addr)
{
	int mi = (uint64_t *)addr - memory;
	if (mi < 0 || 127 < mi) return -1;

	int i, sz, size; bool flag;
	for (i = mi + 127, sz = 8, flag = false; i; i = PARENT(i), sz <<= 1) {
		if (alloc_info[i].allocated) {
			alloc_info[i].allocated = false;
			flag = true; size = sz; }
		if (flag) alloc_info[i].used -= size; }
	if (flag) alloc_info[i].used -= size;
	return 0;
}

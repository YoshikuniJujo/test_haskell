#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <mem-alloc.h>
#include <mem-alloc-local.h>

typedef struct { bool allocated; int used; } AllocInfo;

AllocInfo alloc_info[TREE_SIZE(8)];
uint64_t memory[1 << 7];

void*
allocate_memory(int sz)
{
	int i = get_index(normalize(sz), 0, 0, 0);

	if (i < 0) return NULL; else return memory + i;
}

int
get_index(int sz, int dp, int i, int ret)
{
	AllocInfo *inf = &(alloc_info[i]);
	AllocInfo *l = &(alloc_info[LEFT_CHILD(i)]);
	AllocInfo *r = &(alloc_info[RIGHT_CHILD(i)]);

	if (inf->used + sz > (1024 >> dp)) {
		while (i) {
			i = PARENT(i);
			alloc_info[i].used -= sz; }
		return -1;
	}

	inf->used += sz;
	if (sz << dp == 1024) {
		inf->allocated = true; return ret; }
	else if (l->used <= r->used) {
		get_index(sz, dp + 1, LEFT_CHILD(i), ret); }
	else {	get_index(sz, dp + 1, RIGHT_CHILD(i), ret + (64 >> dp)); }
}

int
normalize(int sz)
{
	if (sz <= 8) return 8;

	int rslt = 1, tmp = sz - 1;
	while (tmp) { tmp >>= 1; rslt <<= 1; }
	return rslt;
}

int
free_memory(void *addr)
{
	int i = (uint64_t *)addr - memory;
	if (i < 0 || 127 < i) return -1;

	int j; int h; int flag; int sz;
	for (j = i + 127, h = 0, flag = false; j; j = PARENT(j), h++) {
		if (alloc_info[j].allocated) { 
			alloc_info[j].allocated = false;
			flag = true; sz = 8 << h; }
		if (flag) alloc_info[j].used -= sz; }
	if (flag) alloc_info[j].used -= sz;
}

void
draw_memory()
{
	char buf[(1 << 7) + 1];

	int i;
	for (i = 0; i < (1 << 7); i++) buf[i] = '.';
	buf[i] = '\0';

	put_used(0, 128, buf);

	printf("%s\n", buf);
}

void
put_used(int i, int sz, char *buf)
{
	if (sz == 0) return;

	if (alloc_info[i].allocated) {
		for (int j; j < sz; j++) buf[j] = '*'; }
	else {	int hf = sz >> 1;
		put_used(LEFT_CHILD(i), hf, buf);
		put_used(RIGHT_CHILD(i), hf, buf + hf); }
}

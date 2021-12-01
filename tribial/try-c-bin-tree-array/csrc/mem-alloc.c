#include <stdio.h>
#include <mem-alloc.h>

AllocInfo alloc_info[TREE_SIZE(8)];

uint64_t memory[1 << 7];

int
normalize(int sz)
{
	if (sz <= 8) { return 8; }

	int rslt = 1, tmp = sz - 1;
	while (tmp) {
		tmp >>= 1;
		rslt <<= 1; }
	return rslt;
}

int
get_index(int sz, int dp, int i, int ret)
{
	AllocInfo *inf = &(alloc_info[i]);
	AllocInfo *l = &(alloc_info[LEFT_CHILD(i)]);
	AllocInfo *r = &(alloc_info[RIGHT_CHILD(i)]);

	printf("get_index: inf->used = %d\n", inf->used);
	if (inf->used + sz > (1024 >> dp)) {
		while (i) {
			i = PARENT(i);
			alloc_info[i].used -= sz;
		}
		return -1;
	}
	inf->used += sz;
	printf("get_index: l->used = %d\n", l->used);
	printf("get_index: r->used = %d\n", r->used);
	if (sz << dp == 1024) {
		inf->allocated = true;
		return ret;
	} else if (l->used <= r->used) {
		get_index(sz, dp + 1, LEFT_CHILD(i), ret);
	} else {
		get_index(sz, dp + 1, RIGHT_CHILD(i), ret + (64 >> dp));
	}
}

int
free_memory(void *addr)
{
	int i = (uint64_t *)addr - memory;
	if (i < 0 || i > 127) return -1;

	int j = i + 127;

	bool flag = false;
	int h = 0;
	int sz;
	while (j) {
		if (alloc_info[j].allocated) { 
			alloc_info[j].allocated = false;
			sz = 8 << h;
			flag = true;
		}
		if (flag) {
			alloc_info[j].used -= sz;
		}
		j = PARENT(j);
		h++;
	}
	if (flag) alloc_info[j].used -= sz;
}

void*
allocate_memory(int sz)
{
	int sz_ = normalize(sz);
	int i = get_index(sz_, 0, 0, 0);

	printf("%d\n", i);

	return memory + i;
}

void
test_pointer_calc(void)
{
	uint64_t foo[10];

	printf("%p\n", foo);
	printf("%p\n", foo + 1);

	printf("%p\n", &(foo[2]));

	int x = &(foo[2]) - foo;

	printf("%d\n", x);
}

void
putUsed(int i, int n, char *buf)
{
	if (n == 0) return;

	if (alloc_info[i].allocated) {
		for (int j; j < n; j++) buf[j] = '*';
	} else {
		putUsed(LEFT_CHILD(i), n >> 1, buf);
		putUsed(RIGHT_CHILD(i), n >> 1, buf + (n >> 1));
	}
}

void
draw_memory()
{
	char buf[(1 << 7) + 1];

	int i;
	for (i = 0; i < (1 << 7); i++) buf[i] = '.';
	buf[i] = '\0';

	putUsed(0, 128, buf);

	printf("%s\n", buf);
}

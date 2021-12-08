#include <stdio.h>
#include <mem_alloc_local.h>

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

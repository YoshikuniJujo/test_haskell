#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <sys/select.h>

#include <human.h>

int hm_left(int x, int y) { return x; }
int hm_right(int x, int y) { return x + 2; }
int hm_top(int x, int y) { return y; }
int hm_bottom(int x, int y) { return y + 2; }

int hm_x_from_left(int l) { return l; }
int hm_x_from_right(int r) { return r - 2; }
int hm_y_from_top(int t) { return t; }
int hm_y_from_bottom (int b) { return b - 2; }

HmFieldArray hm_field0;

void
hm_field0_init(void)
{
	for (int i = 0; i < FIELD_HEIGHT; i++) {
		int j;
		for (j = 0; j < FIELD_WIDTH; j++) hm_field0[i][j] = '.';
		hm_field0[i][j] = '\0';
	}
}

void
hm_field0_draw(void)
{
	for (int i = 0; i < FIELD_HEIGHT; i++) printf("%s\n", hm_field0[i]);
}

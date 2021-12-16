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

void
hm_field0_put_char(int x, int y, char c)
{
	if (0 <= x && x < FIELD_WIDTH && 0 <= y && y < FIELD_HEIGHT)
		hm_field0[y][x] = c;
}

HmPutHumanResult
hm_check_inside(int x, int y)
{
	if (	0 <= hm_left(x, y) && hm_right(x, y) < FIELD_WIDTH &&
		0 <= hm_top(x, y) && hm_bottom(x, y) < FIELD_HEIGHT )
		return HM_PUT_HUMAN_SUCCESS;
	else if (
		0 <= hm_right(x, y) && hm_left(x, y) < FIELD_WIDTH &&
		0 <= hm_bottom(x, y) && hm_top(x, y) < FIELD_HEIGHT )
		return HM_PUT_HUMAN_PARTIAL;
	else	return HM_PUT_HUMAN_OFFSCREEN;
}

HmPutHumanResult
hm_field0_draw_human(int x, int y)
{
	hm_field0_put_char(x, y, '\\');
	hm_field0_put_char(x + 1, y, 'o');
	hm_field0_put_char(x + 1, y + 1, 'A');
	hm_field0_put_char(x + 2, y + 1, '\\');
	hm_field0_put_char(x, y + 2, '/');
	hm_field0_put_char(x + 2, y + 2, '\\');
	hm_field0_draw();
	return hm_check_inside(x, y);
}

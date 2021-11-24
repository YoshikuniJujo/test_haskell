#include <stdio.h>
#include <human.h>

int
hm_left(int x, int y) { return x; }

int
hm_right(int x, int y) { return x + 2; }

int
hm_top(int x, int y) { return y; }

int
hm_bottom(int x, int y) { return y + 2; }

Field hm_field;

void
hm_init_field(void) {
	for (int i = 0; i < 22; i++) {
		int j;
		for (j = 0; j < 79; j++) {
			hm_field[i][j] = '.';
		}
		hm_field[i][j] = '\0';
	}
}

void
hm_draw_field(void) {
	for (int i = 0; i < 22; i++) {
		printf("%s\n", hm_field[i]);
	}
}

HmDrawHumanResult
hm_draw_human(int x, int y) {
	hm_field[y][x] = '\\';
	hm_field[y][x + 1] = 'o';
	hm_field[y + 1][x + 1] = 'A';
	hm_field[y + 1][x + 2] = '\\';
	hm_field[y + 2][x] = '/';
	hm_field[y + 2][x + 2] = '\\';
	hm_draw_field();

	return 0;
}

#include <stdio.h>
#include <human.h>

int
left(int x, int y) { return x; }

int
right(int x, int y) { return x + 2; }

int
top(int x, int y) { return y; }

int
bottom(int x, int y) { return y + 2; }

Field field;

void
init_field(void) {
	for (int i = 0; i < 22; i++) {
		int j;
		for (j = 0; j < 79; j++) {
			field[i][j] = '.';
		}
		field[i][j] = '\0';
	}
}

void
draw_field(void) {
	for (int i = 0; i < 22; i++) {
		printf("%s\n", field[i]);
	}
}

int
draw_human(int x, int y) {
	field[y][x] = '\\';
	field[y][x + 1] = 'o';
	field[y + 1][x + 1] = 'A';
	field[y + 1][x + 2] = '\\';
	field[y + 2][x] = '/';
	field[y + 2][x + 2] = '\\';
	draw_field();

	return 0;
}

#ifndef _HUMAN_H
#define _HUMAN_H

int left(int x, int y);
int right(int x, int y);
int top(int x, int y);
int bottom(int x, int y);

void init_field(void);

typedef enum {
	DRAW_HUMAN_SUCCESS,
	DRAW_HUMAN_PARTIAL,
	DRAW_HUMAN_OFFSCREEN }
	DrawHumanResult;

DrawHumanResult draw_human(int x, int y);

typedef char Field[23][80];

#endif

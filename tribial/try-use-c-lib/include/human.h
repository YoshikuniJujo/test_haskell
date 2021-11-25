#ifndef _HUMAN_H
#define _HUMAN_H

int hm_left(int x, int y);
int hm_right(int x, int y);
int hm_top(int x, int y);
int hm_bottom(int x, int y);

void hm_field0_init(void);

typedef enum {
	HM_DRAW_HUMAN_SUCCESS,
	HM_DRAW_HUMAN_PARTIAL,
	HM_DRAW_HUMAN_OFFSCREEN }
	HmDrawHumanResult;

HmDrawHumanResult hm_field0_draw_human(int x, int y);

#define FIELD_WIDTH	79
#define FIELD_HEIGHT	23

typedef char Field[FIELD_HEIGHT][FIELD_WIDTH + 1];

#endif

#ifndef _HUMAN_H
#define _HUMAN_H

int hm_left(int x, int y);
int hm_right(int x, int y);
int hm_top(int x, int y);
int hm_bottom(int x, int y);

void hm_init_field(void);

typedef enum {
	HM_DRAW_HUMAN_SUCCESS,
	HM_DRAW_HUMAN_PARTIAL,
	HM_DRAW_HUMAN_OFFSCREEN }
	HmDrawHumanResult;

HmDrawHumanResult hm_draw_human(int x, int y);

typedef char Field[23][80];

#endif

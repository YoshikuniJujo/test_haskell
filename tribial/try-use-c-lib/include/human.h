#ifndef _HUMAN_H
#define _HUMAN_H

int hm_left(int x, int y);
int hm_right(int x, int y);
int hm_top(int x, int y);
int hm_bottom(int x, int y);

void hm_field0_init(void);

typedef enum {
	HM_PUT_HUMAN_SUCCESS,
	HM_PUT_HUMAN_PARTIAL,
	HM_PUT_HUMAN_OFFSCREEN }
	HmPutHumanResult;

HmPutHumanResult hm_field0_draw_human(int x, int y);

#define FIELD_WIDTH	79
#define FIELD_HEIGHT	23

typedef char HmFieldArray[FIELD_HEIGHT * (FIELD_WIDTH + 1)];
typedef char *HmField;

HmField hm_field_new(void);
void hm_field_destroy(HmField f);
void hm_field_draw(HmField f);
HmPutHumanResult hm_field_put_human(HmField f, int x, int y);

/*
typedef char HmImageArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char *HmImage;

HmImage hm_field_get_image(HmField f);
*/

#endif

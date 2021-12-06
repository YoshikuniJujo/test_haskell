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

typedef char HmFieldArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char (*HmField)[FIELD_WIDTH + 1];

HmField hm_field_new(void);
void hm_field_clear(HmField f);
void hm_field_destroy(HmField f);
void hm_field_draw(HmField f);
HmPutHumanResult hm_field_put_human(HmField f, int x, int y);

typedef char HmImageArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char (*HmImage)[FIELD_WIDTH + 1];

HmImage hm_field_get_image(HmField f);

typedef enum { HM_SMALL_HEAD, HM_LARGE_HEAD } HmHead;
typedef enum { HM_DOWN_ARM, HM_UP_ARM } HmArm;
typedef struct { HmHead head_size; HmArm left_arm; HmArm right_arm; } HmHuman;

HmPutHumanResult
hm_field_put_various_human(HmField f, HmHuman *hm, int x, int y);

HmHuman *hm_human_copy(HmHuman *hm);
void hm_human_free(HmHuman *hm);

void flipHead(HmHuman *hm);
void flipLeftArm(HmHuman *hm);
void flipRightArm(HmHuman *hm);

typedef enum {
	HM_EVENT_TYPE_TICK,
	HM_EVENT_TYPE_CHAR
	} HmEventType;

typedef struct {
	HmEventType event_type;
	} HmEventAny;

typedef struct {
	HmEventType event_type;
	int times;
	} HmEventTick;

typedef struct {
	HmEventType event_type;
	char character;
	} HmEventChar;

typedef union {
	HmEventTick event_tick;
	HmEventChar event_char;
	} HmEvent;

#endif

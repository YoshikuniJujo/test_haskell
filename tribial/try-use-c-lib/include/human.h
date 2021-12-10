#ifndef _HUMAN_H
#define _HUMAN_H

#include <stdbool.h>

// FIELD

#define FIELD_WIDTH	79
#define FIELD_HEIGHT	23

typedef char HmFieldArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char (*HmField)[FIELD_WIDTH + 1];

// SHAPE OF HUMAN

int hm_left(int x, int y);
int hm_right(int x, int y);
int hm_top(int x, int y);
int hm_bottom(int x, int y);

int hm_x_from_left(int l);
int hm_x_from_right(int r);
int hm_y_from_top(int t);
int hm_y_from_bottom(int b);

// FIELD 0

void hm_field0_init(void);

typedef enum {
	HM_PUT_HUMAN_SUCCESS, HM_PUT_HUMAN_PARTIAL, HM_PUT_HUMAN_OFFSCREEN }
	HmPutHumanResult;

HmPutHumanResult hm_field0_put_human(int x, int y);

// MULTIPLE FIELDS

HmField hm_field_new(void);
void hm_field_clear(HmField f);
void hm_field_destroy(HmField f);
void hm_field_draw(HmField f);
HmPutHumanResult hm_field_put_human(HmField f, int x, int y);

// IMAGE

typedef char HmImageArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char (*HmImage)[FIELD_WIDTH + 1];

HmImage hm_field_get_image(HmField f);
void hm_image_destroy(HmImage img);
void hm_image_draw(HmImage img);

// VARIOUS HUMANS

typedef enum { HM_SMALL_HEAD, HM_LARGE_HEAD } HmHead;
typedef enum { HM_DOWN_ARM, HM_UP_ARM } HmArm;
typedef struct { HmHead head_size; HmArm left_arm; HmArm right_arm; } HmHuman;

HmPutHumanResult
	hm_field_put_various_human(HmField f, HmHuman *hm, int x, int y);

HmHuman *hm_human_copy(HmHuman *hm);
void hm_human_free(HmHuman *hm);

void hm_human_flip_head(HmHuman *hm);
void hm_human_flip_left_arm(HmHuman *hm);
void hm_human_flip_right_arm(HmHuman *hm);

// EVENT

typedef enum { HM_EVENT_TYPE_TICK, HM_EVENT_TYPE_CHAR } HmEventType;

typedef struct { HmEventType event_type; } HmEventAny;
typedef struct { HmEventType event_type; int times; } HmEventTick;
typedef struct { HmEventType event_type; char character; } HmEventChar;

typedef union {
	HmEventAny event_any; HmEventTick event_tick; HmEventChar event_char; }
	HmEvent;

HmEvent *hm_get_event(char (*get_char)());
void hm_event_destroy(HmEvent *ev);

// BACKGROUND

HmField (*hm_field_new_background(bool b)) ();
void (*hm_field_clear_background(bool b)) (HmField);

// MESSAGE

typedef struct { int x; int y; } HmPosition;
typedef struct { HmPosition *position; char *message; } HmMessage;

void hm_field_put_message(HmField f, HmMessage *msg);

#endif

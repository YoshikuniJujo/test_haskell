#ifndef _HUMAN_H
#define _HUMAN_H

#define FIELD_WIDTH 79
#define FIELD_HEIGHT 23

typedef char HmFieldArray[FIELD_HEIGHT][FIELD_WIDTH + 1];
typedef char (*HmField)[FIELD_WIDTH + 1];

typedef enum {
	HM_PUT_HUMAN_SUCCESS, HM_PUT_HUMAN_PARTIAL, HM_PUT_HUMAN_OFFSCREEN }
	HmPutHumanResult;

typedef enum { HM_SMALL_HEAD, HM_LARGE_HEAD } HmHead;
typedef enum { HM_DOWN_ARM, HM_UP_ARM } HmArm;
typedef struct { HmHead head_size; HmArm left_arm; HmArm right_arm; } HmHuman;

typedef enum { HM_EVENT_TYPE_TICK, HM_EVENT_TYPE_CHAR } HmEventType;

typedef struct { HmEventType event_type; } HmEventAny;
typedef struct { HmEventType event_type; int times; } HmEventTick;
typedef struct { HmEventType event_type; char character; } HmEventChar;

typedef union {
	HmEventAny event_any; HmEventTick event_tick; HmEventChar event_char; }
	HmEvent;

typedef struct { int x; int y; } HmPosition;
typedef struct { HmPosition *position; char *message; } HmMessage;

#endif

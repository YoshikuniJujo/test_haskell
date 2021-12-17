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

#endif

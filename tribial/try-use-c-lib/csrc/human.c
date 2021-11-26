#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <human.h>

int
hm_left(int x, int y) { return x; }

int
hm_right(int x, int y) { return x + 2; }

int
hm_top(int x, int y) { return y; }

int
hm_bottom(int x, int y) { return y + 2; }

HmFieldArray hm_field0;

void
hm_field0_init(void) {
	for (int i = 0; i < FIELD_HEIGHT; i++) {
		int j;
		for (j = 0; j < FIELD_WIDTH; j++) {
			hm_field0[i][j] = '.';
		}
		hm_field0[i][j] = '\0';
	}
}

void
hm_field0_draw(void) {
	for (int i = 0; i < FIELD_HEIGHT; i++) {
		printf("%s\n", hm_field0[i]);
	}
}

HmPutHumanResult
hm_check_inside(int x, int y) {
	if (	0 <= hm_left(x, y) && hm_right(x, y) < FIELD_WIDTH &&
		0 <= hm_top(x, y) && hm_bottom(x, y) < FIELD_HEIGHT ) {
		return HM_PUT_HUMAN_SUCCESS; }
	else if (	(0 <= hm_right(x, y) && hm_left(x, y) < FIELD_WIDTH) &&
			(0 <= hm_bottom(x, y) && hm_top(x, y) < FIELD_HEIGHT) ) {
		return HM_PUT_HUMAN_PARTIAL; }
	else {	return HM_PUT_HUMAN_OFFSCREEN; }
}

void
hm_field0_set_char(int x, int y, char c)
{
	if (0 <= x && x < FIELD_WIDTH && 0 <= y && y < FIELD_HEIGHT) {
		hm_field0[y][x] = c;
	}
}

HmPutHumanResult
hm_field0_draw_human(int x, int y) {
	hm_field0_set_char(x, y, '\\');
	hm_field0_set_char(x + 1, y, 'o');
	hm_field0_set_char(x + 1, y + 1, 'A');
	hm_field0_set_char(x + 2, y + 1, '\\');
	hm_field0_set_char(x, y + 2, '/');
	hm_field0_set_char(x + 2, y + 2, '\\');
	hm_field0_draw();

	return hm_check_inside(x, y);
}

HmField
hm_field_new(void)
{
	HmField f;
	f = (HmField)malloc(sizeof(HmFieldArray));

	for (int i = 0; i < FIELD_HEIGHT; i++) {
		int j;
		for (j = 0; j < FIELD_WIDTH; j++) {
			f[i][j] = '.';
		}
		f[i][j] = '\0';
	}

	return f;
}

void
hm_field_draw(HmField f)
{
	for (int i = 0; i < FIELD_HEIGHT; i++) {
		printf("%s\n", f[i]);
	}
}

void
hm_field_destroy(HmField f)
{
	free(f);
}

void
hm_field_set_char(HmField f, int x, int y, char c)
{
	if (0 <= x && x < FIELD_WIDTH && 0 <= y && y < FIELD_HEIGHT) {
		f[y][x] = c;
	}
}

HmPutHumanResult
hm_field_put_human(HmField f, int x, int y)
{
	hm_field_set_char(f, x, y, '\\');
	hm_field_set_char(f, x + 1, y, 'o');
	hm_field_set_char(f, x + 1, y + 1, 'A');
	hm_field_set_char(f, x + 2, y + 1, '\\');
	hm_field_set_char(f, x, y + 2, '/');
	hm_field_set_char(f, x + 2, y + 2, '\\');

	return hm_check_inside(x, y);
}

HmImage
hm_field_get_image(HmField f)
{
	HmImage img;

	img = (HmImage)malloc(sizeof(HmImageArray));
	memcpy(img, f, (FIELD_WIDTH + 1) * FIELD_HEIGHT);

	return img;
}

void
hm_image_draw(HmImage img)
{
	for (int i = 0; i < FIELD_HEIGHT; i++) {
		printf("%s\n", img[i]);
	}
}

void
hm_image_destroy(HmImage img)
{
	free(img);
}

char
select_head(HmHuman *hm)
{
	switch (hm->head_size) {
		case HM_SMALL_HEAD: return 'o';
		case HM_LARGE_HEAD: return 'O'; }
}

void
put_left_arm(HmField f, HmHuman *hm, int x, int y)
{
	switch (hm->left_arm) {
		case HM_DOWN_ARM: hm_field_set_char(f, x, y + 1, '/'); return;
		case HM_UP_ARM: hm_field_set_char(f, x, y, '\\'); return; }
}

void
put_right_arm(HmField f, HmHuman *hm, int x, int y)
{
	switch (hm->right_arm) {
		case HM_DOWN_ARM:
			hm_field_set_char(f, x + 2, y + 1, '\\'); return;
		case HM_UP_ARM: hm_field_set_char(f, x + 2, y, '/'); return;
	}
}

HmPutHumanResult
hm_field_put_various_human(HmField f, HmHuman *hm, int x, int y)
{
	hm_field_set_char(f, x + 1, y, select_head(hm));
	put_left_arm(f, hm, x, y);
	put_right_arm(f, hm, x, y);
	hm_field_set_char(f, x + 1, y + 1, 'A');
	hm_field_set_char(f, x, y + 2, '/');
	hm_field_set_char(f, x + 2, y + 2, '\\');

	return hm_check_inside(x, y);
}

HmHuman*
hm_human_copy(HmHuman *hm)
{
	HmHuman *dst;

	dst = (HmHuman *)malloc(sizeof(HmHuman));
	dst->head_size = hm->head_size;
	dst->left_arm = hm->left_arm;
	dst->right_arm = hm->right_arm;

	return dst;
}

void
hm_human_destroy(HmHuman *hm)
{
	free(hm);
}

void
hm_human_flip_head(HmHuman *hm)
{
	hm->head_size =
		hm->head_size == HM_SMALL_HEAD ? HM_LARGE_HEAD : HM_SMALL_HEAD;
}

void
hm_human_flip_left_arm(HmHuman *hm)
{
	hm->left_arm =
		hm->left_arm == HM_DOWN_ARM ? HM_UP_ARM : HM_DOWN_ARM;
}

void
hm_human_flip_right_arm(HmHuman *hm)
{
	hm->right_arm =
		hm->right_arm == HM_DOWN_ARM ? HM_UP_ARM : HM_DOWN_ARM;
}

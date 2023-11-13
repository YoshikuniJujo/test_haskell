#pragma once

#include <gtk/gtk.h>

#define TFE_TYPE_TEXT_VIEW tfe_text_view_get_type()
G_DECLARE_FINAL_TYPE(TfeTextView, tfe_text_view, TFE, TEXT_VIEW, GtkTextView)

enum TfeTextViewOpenResponseType {
	TFE_OPEN_RESPONSE_SUCCESS,
	TFE_OPEN_RESPONSE_CANCEL,
	TFE_OPEN_RESPONSE_ERROR };

void
tfe_text_view_set_file(TfeTextView *tv, GFile *f);

GFile *
tfe_text_view_get_file(TfeTextView *tv);

void
tfe_text_view_open(TfeTextView *tv, GtkWindow *win);

void
tfe_text_view_save(TfeTextView *tv);

void
tfe_text_view_saveas(TfeTextView *tv);

GtkWidget *
tfe_text_view_new_with_file(GFile *file);

GtkWidget *
tfe_text_view_new(void);

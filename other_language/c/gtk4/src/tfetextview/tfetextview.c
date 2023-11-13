#include <gtk/gtk.h>
#include "tfetextview.h"

struct _TfeTextView
{
	GtkTextView parent;
	GFile *file;
};

G_DEFINE_FINAL_TYPE(TfeTextView, tfe_text_view, GTK_TYPE_TEXT_VIEW);

static void
tfe_text_view_init(TfeTextView *tv)
{
}

static void
tfe_text_view_class_init(TfeTextViewClass *class)
{
}

GFile *
tfe_text_view_get_file(TfeTextView *tv)
{
	return tv -> file;
}

GtkWidget *
tfe_text_view_new_with_file(GFile *file)
{
	g_return_val_if_fail(G_IS_FILE(file), NULL);

	GtkWidget *tv;
	GtkTextBuffer *tb;
	char *contents;
	gsize length;

	if (! g_file_load_contents(file, NULL, &contents, &length, NULL, NULL))
		return NULL;

	tv = tfe_text_view_new();
	tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
	gtk_text_buffer_set_text(tb, contents, length);
	TFE_TEXT_VIEW(tv)->file = g_file_dup(file);
	gtk_text_buffer_set_modified(tb, FALSE);
	g_free(contents);

	g_print("here");

	return tv;
}

GtkWidget *
tfe_text_view_new(void)
{
	return GTK_WIDGET(g_object_new(TFE_TYPE_TEXT_VIEW,
				"wrap-mode", GTK_WRAP_WORD_CHAR, NULL));
}

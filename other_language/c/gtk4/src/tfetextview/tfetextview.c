#include <gtk/gtk.h>
#include "tfetextview.h"

struct _TfeTextView
{
	GtkTextView parent;
	GFile *file;
};

G_DEFINE_FINAL_TYPE(TfeTextView, tfe_text_view, GTK_TYPE_TEXT_VIEW);

enum {	CHANGE_FILE,
	OPEN_RESPONSE,
	NUMBER_OF_SIGNALS };

static guint tfe_text_view_signals[NUMBER_OF_SIGNALS];

static void
tfe_text_view_init(TfeTextView *tv)
{
	tv->file = NULL;
}

static void
tfe_text_view_dispose(GObject *gobject)
{
	TfeTextView *tv = TFE_TEXT_VIEW(gobject);

	if (G_IS_FILE(tv->file)) g_clear_object(&tv->file);
	G_OBJECT_CLASS(tfe_text_view_parent_class)->dispose(gobject);
}

static void
tfe_text_view_class_init(TfeTextViewClass *class)
{
	GObjectClass *object_class = G_OBJECT_CLASS(class);

	object_class->dispose = tfe_text_view_dispose;
	tfe_text_view_signals[CHANGE_FILE] = g_signal_new(
			"change-file",
			G_TYPE_FROM_CLASS(class),
			G_SIGNAL_RUN_LAST |
			G_SIGNAL_NO_RECURSE |
			G_SIGNAL_NO_HOOKS,
			0, NULL, NULL, NULL, G_TYPE_NONE, 0 );
	tfe_text_view_signals[OPEN_RESPONSE] = g_signal_new(
			"open-response",
			G_TYPE_FROM_CLASS(class),
			G_SIGNAL_RUN_LAST |
			G_SIGNAL_NO_RECURSE |
			G_SIGNAL_NO_HOOKS,
			0, NULL, NULL, NULL, G_TYPE_NONE, 1, G_TYPE_INT );
}

GFile *
tfe_text_view_get_file(TfeTextView *tv)
{
	g_return_val_if_fail(TFE_IS_TEXT_VIEW(tv), NULL);

	if (G_IS_FILE(tv->file)) return g_file_dup(tv -> file);
	else return NULL;
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

	return tv;
}

GtkWidget *
tfe_text_view_new(void)
{
	return GTK_WIDGET(g_object_new(TFE_TYPE_TEXT_VIEW,
				"wrap-mode", GTK_WRAP_WORD_CHAR, NULL));
}

static gboolean
save_file (GFile *file, GtkTextBuffer *tb, GtkWindow *win)
{
	GtkTextIter start_iter, end_iter;
	char *contents;
	gboolean stat;
	GtkAlertDialog *alert_dialog;
	GError *err = NULL;

	gtk_text_buffer_get_bounds(tb, &start_iter, &end_iter);
	contents = gtk_text_buffer_get_text(tb, &start_iter, &end_iter, FALSE);
	stat = g_file_replace_contents(
			file, contents, strlen(contents), NULL, TRUE,
			G_FILE_CREATE_NONE, NULL, NULL, &err);
	if (stat)
		gtk_text_buffer_set_modified(tb, FALSE);
	else {
		alert_dialog = gtk_alert_dialog_new("%s", err->message);
		gtk_alert_dialog_show(alert_dialog, win);
		g_object_unref(alert_dialog);
		g_error_free(err);
	}
	g_free(contents);
	return stat;
}

static void
save_dialog_cb(GObject *source_object, GAsyncResult *res, gpointer data)
{
	GtkFileDialog *dialog = GTK_FILE_DIALOG(source_object);
	TfeTextView *tv = TFE_TEXT_VIEW(data);
	GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
	GFile *file;
	GtkWidget *win = gtk_widget_get_ancestor(GTK_WIDGET(tv), GTK_TYPE_WINDOW);
	GError *err = NULL;
	GtkAlertDialog *alert_dialog;

	if (((file = gtk_file_dialog_save_finish(dialog, res, &err)) != NULL) &&
			save_file(file, tb, GTK_WINDOW(win))) {
		if (!(G_IS_FILE(tv->file) && g_file_equal(tv->file, file))) {
			if (G_IS_FILE(tv->file))
				g_object_unref(tv->file);
			tv->file = file;
			g_signal_emit(tv, tfe_text_view_signals[CHANGE_FILE], 0);
		}
	}
	if (err) {
		alert_dialog = gtk_alert_dialog_new("%s", err->message);
		gtk_alert_dialog_show(alert_dialog, GTK_WINDOW(win));
		g_object_unref(alert_dialog);
		g_clear_error(&err);
	}
}

void
tfe_text_view_save(TfeTextView *tv)
{
	g_return_if_fail(TFE_IS_TEXT_VIEW(tv));

	GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
	GtkWidget *win = gtk_widget_get_ancestor(GTK_WIDGET(tv), GTK_TYPE_WINDOW);

	if (! gtk_text_buffer_get_modified(tb))
		return;
	else if (tv->file == NULL)
		tfe_text_view_saveas(tv);
	else	save_file(tv->file, tb, GTK_WINDOW(win));
}

void
tfe_text_view_saveas(TfeTextView *tv)
{
	g_return_if_fail(TFE_IS_TEXT_VIEW(tv));

	GtkWidget *win =
		gtk_widget_get_ancestor(GTK_WIDGET(tv), GTK_TYPE_WINDOW);
	GtkFileDialog *dialog;

	dialog = gtk_file_dialog_new();
	gtk_file_dialog_save(dialog, GTK_WINDOW(win), NULL, save_dialog_cb, tv);
	g_object_unref(dialog);
}

static void
open_dialog_cb(GObject *source_object, GAsyncResult *res, gpointer data)
{
	GtkFileDialog *dialog = GTK_FILE_DIALOG(source_object);
	TfeTextView *tv = TFE_TEXT_VIEW(data);
	GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
	GtkWidget *win = gtk_widget_get_ancestor(GTK_WIDGET(tv), GTK_TYPE_WINDOW);
	GFile *file;
	char *contents;
	gsize length;
	gboolean file_changed;
	GtkAlertDialog *alert_dialog;
	GError *err = NULL;

	if ((file = gtk_file_dialog_open_finish(dialog, res, &err)) != NULL &&
		g_file_load_contents(
			file, NULL, &contents, &length, NULL, &err )) {
		gtk_text_buffer_set_text(tb, contents, length);
		g_free(contents);
		gtk_text_buffer_set_modified(tb, FALSE);
		file_changed = (G_IS_FILE (tv->file) &&
			g_file_equal(tv->file, file)) ? FALSE : TRUE;
		if (G_IS_FILE(tv->file)) g_object_unref(tv->file);
		tv->file = file;
		if (file_changed)
			g_signal_emit(tv, tfe_text_view_signals[CHANGE_FILE], 0);
		g_signal_emit(
			tv, tfe_text_view_signals[OPEN_RESPONSE],
			0, TFE_OPEN_RESPONSE_SUCCESS );
	} else {
		if (err->code == GTK_DIALOG_ERROR_DISMISSED)
			g_signal_emit(
				tv, tfe_text_view_signals[OPEN_RESPONSE],
				0, TFE_OPEN_RESPONSE_CANCEL);
		else {
			alert_dialog = gtk_alert_dialog_new("%s", err->message);
			gtk_alert_dialog_show(alert_dialog, GTK_WINDOW(win));
			g_object_unref(alert_dialog);
			g_signal_emit(
				tv, tfe_text_view_signals[OPEN_RESPONSE],
				0, TFE_OPEN_RESPONSE_ERROR);
		}
		g_clear_error(&err);
	}
}

void
tfe_text_view_open(TfeTextView *tv, GtkWindow *win)
{
	g_return_if_fail(TFE_IS_TEXT_VIEW(tv));
	g_return_if_fail(GTK_IS_WINDOW(win) || win == NULL);

	GtkFileDialog *dialog;

	dialog = gtk_file_dialog_new();
	gtk_file_dialog_open(dialog, win, NULL, open_dialog_cb, tv);
	g_object_unref(dialog);
}

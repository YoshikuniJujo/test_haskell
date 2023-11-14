#include <gtk/gtk.h>

#include "../tfetextview/tfetextview.h"

static char*
get_untitled()
{
	static int c = -1;
	if (++c == 0)
		return g_strdup_printf("Untitled");
	else	return g_strdup_printf("Untitled%u", c);
}

static void
file_changed_cb(TfeTextView *tv, GtkNotebook *nb)
{
	GtkWidget *scr;
	GtkWidget *label;
	GFile *file;
	char *filename;

	file = tfe_text_view_get_file(tv);
	scr = gtk_widget_get_parent(GTK_WIDGET(tv));
	if (G_IS_FILE(file)) {
		filename = g_file_get_basename(file);
		g_object_unref(file);
	} else	filename = get_untitled();
	label = gtk_label_new(filename);
	g_free(filename);
	gtk_notebook_set_tab_label(nb, scr, label);
}

static void
notebook_page_build(GtkNotebook *nb, GtkWidget *tv, const char *filename)
{
	GtkWidget *scr = gtk_scrolled_window_new();
	GtkNotebookPage *nbp;
	GtkWidget *lab;
	int i;

	gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scr), tv);
	lab = gtk_label_new(filename);
	i = gtk_notebook_append_page(nb, scr, lab);
	nbp = gtk_notebook_get_page(nb, scr);
	g_object_set(nbp, "tab-expand", TRUE, NULL);
	gtk_notebook_set_current_page(nb, i);
	g_signal_connect(
		GTK_TEXT_VIEW(tv), "change-file",
		G_CALLBACK(file_changed_cb), nb);
}

void
notebook_page_new(GtkNotebook *nb)
{
	g_return_if_fail(GTK_IS_NOTEBOOK(nb));

	GtkWidget *tv;
	char *filename;

	tv = tfe_text_view_new();
	filename = get_untitled();
	notebook_page_build(nb, tv, filename);
	g_free(filename);
}

void
notebook_page_new_with_file(GtkNotebook *nb, GFile *file)
{
	g_return_if_fail(GTK_IS_NOTEBOOK(nb));
	g_return_if_fail(G_IS_FILE(file));

	GtkWidget *tv;
	char *filename;

	if ((tv = tfe_text_view_new_with_file(file)) == NULL)
		return;
	filename = g_file_get_basename(file);
	notebook_page_build(nb, tv, filename);
	g_free(filename);
}

static void
open_response_cb(TfeTextView *tv, int response, GtkNotebook *nb)
{
	GFile *file;
	char *filename;

	if (response != TFE_OPEN_RESPONSE_SUCCESS) {
		g_object_ref_sink(tv);
		g_object_unref(tv);
	} else {
		file = tfe_text_view_get_file(tv);
		filename = g_file_get_basename(file);
		g_object_unref(file);
		notebook_page_build(nb, GTK_WIDGET(tv), filename);
		g_free(filename);
	}
}

void
notebook_page_open(GtkNotebook *nb)
{
	g_return_if_fail(GTK_IS_NOTEBOOK(nb));

	GtkWidget *tv;

	tv = tfe_text_view_new();
	g_signal_connect(
		TFE_TEXT_VIEW(tv), "open-response",
		G_CALLBACK(open_response_cb), nb );
	tfe_text_view_open(
		TFE_TEXT_VIEW(tv),
		GTK_WINDOW(
			gtk_widget_get_ancestor(
				GTK_WIDGET(nb), GTK_TYPE_WINDOW ) ) );
}

void
notebook_page_close(GtkNotebook *nb)
{
	g_return_if_fail(GTK_IS_NOTEBOOK(nb));

	GtkWidget *win;
	int i;

	if (gtk_notebook_get_n_pages(nb) == 1) {
		win = gtk_widget_get_ancestor(GTK_WIDGET(nb), GTK_TYPE_WINDOW);
		gtk_window_destroy(GTK_WINDOW(win));
	} else {
		i = gtk_notebook_get_current_page(nb);
		gtk_notebook_remove_page(GTK_NOTEBOOK(nb), i);
	}
}

static TfeTextView *
get_current_textview(GtkNotebook *nb)
{
	int i;
	GtkWidget *scr;
	GtkWidget *tv;

	i = gtk_notebook_get_current_page(nb);
	scr = gtk_notebook_get_nth_page(nb, i);
	tv = gtk_scrolled_window_get_child(GTK_SCROLLED_WINDOW(scr));
	return TFE_TEXT_VIEW(tv);
}

void
notebook_page_save(GtkNotebook *nb)
{
	g_return_if_fail(GTK_IS_NOTEBOOK(nb));

	TfeTextView *tv;

	tv = get_current_textview(nb);
	tfe_text_view_save(tv);
}

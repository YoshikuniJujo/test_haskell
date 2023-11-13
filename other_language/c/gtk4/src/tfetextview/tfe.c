#include <gtk/gtk.h>
#include "tfetextview.h"

static void
app_activate(GApplication *app)
{
	g_printerr("You need a filename argument.\n");
}

static gboolean
before_close(GtkWindow *win, GtkWidget *nb)
{
	GtkWidget *scr;
	GtkWidget *tv;
	GFile *file;
	GtkTextBuffer *tb;
	GtkTextIter start_iter;
	GtkTextIter end_iter;
	char *contents;
	unsigned int n;
	unsigned int i;
	GError *err = NULL;

	n = gtk_notebook_get_n_pages(GTK_NOTEBOOK(nb));
	for (i = 0; i < n; ++i) {
		scr = gtk_notebook_get_nth_page(GTK_NOTEBOOK(nb), i);
		tv = gtk_scrolled_window_get_child(GTK_SCROLLED_WINDOW(scr));
		file = tfe_text_view_get_file(TFE_TEXT_VIEW(tv));
		tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
		gtk_text_buffer_get_bounds(tb, &start_iter, &end_iter);
		contents = gtk_text_buffer_get_text(
				tb, &start_iter, &end_iter, FALSE);
		if (!g_file_replace_contents(
				file, contents, strlen(contents), NULL, TRUE,
				G_FILE_CREATE_NONE, NULL, NULL, &err)) {
			g_printerr("%s.\n", err->message);
			g_clear_error(&err); }
		g_free(contents);
		g_object_unref(file);
	}

	return FALSE;
}

static void
app_open(GApplication *app, GFile *files[], int n_files, char *hint)
{
	GtkWidget *win;

	GtkWidget *nb;
	GtkWidget *lab;

	GtkWidget *scr;
	GtkWidget *tv;

	char *contents;
	gsize length;
	char *filename;
	int i;
	GError *err = NULL;

	GtkBuilder *build;

	build = gtk_builder_new_from_resource("/com/github/ToshioCP/tfe/tfe.ui");
	win = GTK_WIDGET(gtk_builder_get_object(build, "win"));
	gtk_window_set_application(GTK_WINDOW(win), GTK_APPLICATION(app));
	nb = GTK_WIDGET(gtk_builder_get_object(build, "nb"));
	g_object_unref(build);

	for (i = 0; i < n_files; i++) {
		if (g_file_load_contents(files[i], NULL, &contents, &length, NULL, &err)) {
			scr = gtk_scrolled_window_new();
			tv = tfe_text_view_new_with_file(files[i]);
			gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scr), tv);
			if ((filename = g_file_get_basename(files[i])) != NULL) {
				lab = gtk_label_new(filename);
				g_free(filename);
			} else	lab = gtk_label_new("");
			gtk_notebook_append_page(GTK_NOTEBOOK(nb), scr, lab);
		} else {
			g_printerr("%s.\n", err->message);
			g_error_free(err);
		}
	}
	if (gtk_notebook_get_n_pages(GTK_NOTEBOOK(nb)) > 0) {
		g_signal_connect(win, "close-request", G_CALLBACK(before_close), nb);
		gtk_window_present(GTK_WINDOW(win));
	} else	gtk_window_destroy(GTK_WINDOW(win));

}

int
main(int argc, char *argv[])
{

	GtkApplication *app;
	int stat;

	app = gtk_application_new(
		"com.github.ToshioCP.tfvv2", G_APPLICATION_HANDLES_OPEN );
	g_signal_connect(app, "activate", G_CALLBACK(app_activate), NULL);
	g_signal_connect(app, "open", G_CALLBACK(app_open), NULL);
	stat = g_application_run(G_APPLICATION(app), argc, argv);
	g_object_unref(app);

	return stat;

}

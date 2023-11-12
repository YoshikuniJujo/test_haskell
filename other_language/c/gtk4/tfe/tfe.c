#include <gtk/gtk.h>

static void
app_activate(GApplication *app)
{
	g_printerr("You need a filename argument.\n");
}

static void
app_open(GApplication *app, GFile *files[], int n_files, char *hint)
{
	GtkWidget *win;

	GtkWidget *nb;
	GtkWidget *lab;
	GtkNotebookPage *nbp;

	GtkWidget *scr;
	GtkWidget *tv;
	GtkTextBuffer *tb;

	char *contents;
	gsize length;
	char *filename;
	int i;
	GError *err = NULL;

	win = gtk_application_window_new(GTK_APPLICATION(app));
	gtk_window_set_title(GTK_WINDOW(win), "file viewr");
	gtk_window_set_default_size(GTK_WINDOW(win), 600, 400);
	nb = gtk_notebook_new();
	lab = gtk_label_new("TEST");

	scr = gtk_scrolled_window_new();
	gtk_window_set_child(GTK_WINDOW(win), nb);
	gtk_notebook_append_page(GTK_NOTEBOOK(nb), scr, lab);

	for (i = 0; i < n_files; i++) {
		g_print("%d\n", i);
	}

	tv = gtk_text_view_new();
	tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));

	if (g_file_load_contents(files[0], NULL, &contents, &length, NULL, &err)) {
		gtk_text_buffer_set_text(tb, contents, length);
		g_free(contents);

		if ((filename = g_file_get_basename(files[0])) != NULL) {
			g_free(filename);
		}
		gtk_window_present(GTK_WINDOW(win));
	} else {
		g_printerr("%s.\n", err->message);
		g_error_free(err);
		gtk_window_destroy(GTK_WINDOW(win));
	}

	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(tv), GTK_WRAP_WORD_CHAR);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(tv), FALSE);
	gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scr), tv);

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

#include <gtk/gtk.h>

#define TFE_TYPE_TEXT_VIEW tfe_text_view_get_type()
G_DECLARE_FINAL_TYPE(TfeTextView, tfe_text_view, TFE, TEXT_VIEW, GtkTextView);

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

void
tfe_text_view_set_file(TfeTextView *tv, GFile *f)
{
	tv -> file = f;
}

GFile *
tfe_text_view_get_file(TfeTextView *tv)
{
	return tv -> file;
}

GtkWidget *
tfe_text_view_new(void)
{
	return GTK_WIDGET(g_object_new(TFE_TYPE_TEXT_VIEW, NULL));
}

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
	char *pathname;
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
	GtkNotebookPage *nbp;

	GtkWidget *scr;
	GtkWidget *tv;
	GtkTextBuffer *tb;

	char *contents;
	gsize length;
	char *filename;
	int i;
	GError *err = NULL;

	GtkBuilder *build;

	build = gtk_builder_new_from_file("tfe.ui");
	win = GTK_WIDGET(gtk_builder_get_object(build, "win"));
	gtk_window_set_application(GTK_WINDOW(win), GTK_APPLICATION(app));
	nb = GTK_WIDGET(gtk_builder_get_object(build, "nb"));
	g_object_unref(build);

	for (i = 0; i < n_files; i++) {
		if (g_file_load_contents(files[i], NULL, &contents, &length, NULL, &err)) {
			scr = gtk_scrolled_window_new();
			tv = tfe_text_view_new();
			tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
			gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(tv), GTK_WRAP_WORD_CHAR);
			gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(scr), tv);

			tfe_text_view_set_file(TFE_TEXT_VIEW(tv), g_file_dup(files[i]));
			gtk_text_buffer_set_text(tb, contents, length);
			g_free(contents);
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

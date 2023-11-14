#include <gtk/gtk.h>
#include <math.h>
#include <time.h>

static void
app_activate(GApplication *app, gpointer user_data)
{
	GtkWidget *win;
	GtkWidget *clock;
	GtkBuilder *build;

	build = gtk_builder_new_from_resource("/com/github/ToshioCP/tfc/tfc.ui");
	win = GTK_WIDGET(gtk_builder_get_object(build, "win"));
	gtk_window_set_application(GTK_WINDOW(win), GTK_APPLICATION(app));

	gtk_window_present(GTK_WINDOW(win));
}

static void
app_open(
	GApplication *app, GFile **files,
	gint n_files, gchar *hint, gpointer user_data )
{
	app_activate(app, user_data);
}

int
main(int argc, char *argv[])
{
	GtkApplication *app;
	int stat;

	app = gtk_application_new(
		"com.github.ToshioCP.tfc", G_APPLICATION_HANDLES_OPEN );
	g_signal_connect(app, "activate", G_CALLBACK(app_activate), NULL);
	g_signal_connect(app, "open", G_CALLBACK(app_open), NULL);
	stat = g_application_run(G_APPLICATION(app), argc, argv);
	g_object_unref(app);

	return stat;
}

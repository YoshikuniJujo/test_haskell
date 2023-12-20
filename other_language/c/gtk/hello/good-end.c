#include <gtk/gtk.h>

static gboolean
cb_delete(GtkWidget *widget, gpointer user_data)
{
	return FALSE;
}

int
main(int argc, char *argv[])
{
	GtkWidget *window;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "ハロー");

	g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(cb_delete), NULL);

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}

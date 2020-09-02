#include <stdio.h>
#include <gtk/gtk.h>

gboolean
button_press(GtkWidget *w, GdkEventButton *e, gpointer p)
{
	printf("%p, %p, %p\n", w, e, p);
	return FALSE;
}

gboolean
key_press(GtkWidget *w, GdkEventKey *e, gpointer p)
{
	printf("%p, %p, %p\n", w, e, p);
	return FALSE;
}

int
main(int argc, char *argv[])
{
	GtkWidget *window;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(window, "button-press-event", G_CALLBACK(button_press), NULL);
	g_signal_connect(window, "key-press-event", G_CALLBACK(key_press), NULL);

	gtk_widget_show_all(window);
	gtk_main();
	return 0;
}

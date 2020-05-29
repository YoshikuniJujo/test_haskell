#include <gtk/gtk.h>
#include <stdio.h>

gint
cb_motion_notify_event(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
	gdouble pressure = 0;

	printf("MOTION EVENT OCCUR\n");
	if (gdk_event_get_axis ((GdkEvent *)event, GDK_AXIS_PRESSURE, &pressure)) {
		printf("PEN PRESSURE\n");
	} else {
		printf("NO PEN PRESSURE\n");
	}
	return TRUE;
}

int
main(int argc, char *argv[])
{
	GtkWidget *window;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	gtk_window_set_title(GTK_WINDOW(window), "ハロー");
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(window, "motion_notify_event", G_CALLBACK(cb_motion_notify_event), NULL);
	gtk_widget_set_events(window, GDK_POINTER_MOTION_MASK);

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}

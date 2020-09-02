#include <gtk/gtk.h>

gboolean
expose_event(GtkWidget *w, GdkEventExpose *e)
{
	printf("%p, %p\n", w, e);
	return TRUE;
}

gboolean
motion_notify_event(GtkWidget *w, GdkEventMotion *e)
{
	printf("%p, %p\n", w, e);
	printf("%f, %f\n", e -> x, e -> y);
	printf("%f\n", e -> x * e -> y);
	gtk_widget_queue_draw(w);
	return TRUE;
}

gboolean
draw(GtkWidget *w, cairo_t *cr)
{
	cairo_move_to(cr, 100, 100);
	cairo_line_to(cr, 400, 300);
	cairo_stroke(cr);
}

int
main(int argc, char *argv[])
{
	GtkWidget *w, *da;

	gtk_init(&argc, &argv);

	w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_events(w, GDK_POINTER_MOTION_MASK);
	g_signal_connect(w, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(w, "motion-notify-event", G_CALLBACK(motion_notify_event), NULL);

	da = gtk_drawing_area_new();
	g_signal_connect(da, "draw", G_CALLBACK(draw), NULL);

	gtk_container_add(GTK_CONTAINER(w), da);

	gtk_widget_show_all(w);
	gtk_main();
	return 0;
}

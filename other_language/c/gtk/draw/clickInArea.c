#include <gtk/gtk.h>

static gboolean
clicked(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	printf("Clicked!\n");

	return TRUE;
}

void
draw_callback(GtkWidget *da, cairo_t *cr)
{
	cairo_move_to(cr, 150, 100);
	cairo_line_to(cr, 300, 200);
	cairo_stroke(cr);
}

int
main(int argc, char *argv[])
{

	gtk_init(&argc, &argv);

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	GtkWidget *da = gtk_drawing_area_new();
	gtk_container_add(GTK_CONTAINER(window), da);
	g_signal_connect(da, "draw", G_CALLBACK(draw_callback), NULL);
	gtk_widget_add_events(da, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(da, "button-press-event", G_CALLBACK(clicked), NULL);

	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

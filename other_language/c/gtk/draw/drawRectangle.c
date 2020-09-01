#include <stdio.h>
#include <gtk/gtk.h>

void draw_callback(GtkWidget *, cairo_t *);

int
main(int argc, char *argv[])
{
	GtkWidget *window, *da;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	da = gtk_drawing_area_new();
	gtk_container_add(GTK_CONTAINER(window), da);
	g_signal_connect(da, "draw", G_CALLBACK(draw_callback), NULL);

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}

void
draw_callback(GtkWidget *da, cairo_t *cr)
{
	GdkRGBA color;

	cairo_move_to(cr, 0, 10);
	cairo_line_to(cr, 300, 10);
	cairo_stroke(cr);

	cairo_set_source_rgb(cr, 1.0, 0.0, 0.0);
	cairo_rectangle(cr, 50.0, 50.0, 300.0, 200.0);
	cairo_stroke_preserve(cr);
	cairo_fill(cr);
}

#include <gtk/gtk.h>

void
draw_callback(GtkWidget *da, cairo_t *cr, cairo_surface_t *pi)
{
	cairo_move_to(cr, 100, 100);
	cairo_line_to(cr, 400, 300);
	cairo_stroke(cr);

	cairo_set_source_surface(cr, pi, 200, 200);
	cairo_paint(cr);
}

int
main(int argc, char *argv[])
{
	GtkWidget *w, *da;
	cairo_surface_t *pngimage;

	pngimage = cairo_image_surface_create_from_png("saikoro.png");

	gtk_init(&argc, &argv);

	w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(w, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	da = gtk_drawing_area_new();
	gtk_container_add(GTK_CONTAINER(w), da);
	g_signal_connect(da, "draw", G_CALLBACK(draw_callback), pngimage);

	gtk_widget_show_all(w);
	gtk_main();

	cairo_surface_destroy(pngimage);

	return 0;
}

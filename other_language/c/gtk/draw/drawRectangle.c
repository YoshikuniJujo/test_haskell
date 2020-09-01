#include <stdio.h>
#include <gtk/gtk.h>

void draw_callback(GtkWidget *, cairo_t *);
static gint configure_callback(GtkWidget *, GdkEventConfigure *, GdkPixbuf **);

gboolean
foo (gpointer user_data)
{
	printf("hello\n");
}

int
main(int argc, char *argv[])
{
	GtkWidget *window, *da;
	GdkPixbuf *pb;
//	GdkPixmap *pm;

	gtk_init(&argc, &argv);

	g_timeout_add(10000, (GSourceFunc) foo, NULL);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	da = gtk_drawing_area_new();
	gtk_container_add(GTK_CONTAINER(window), da);
	g_signal_connect(da, "draw", G_CALLBACK(draw_callback), NULL);
	g_signal_connect(da, "configure_event", G_CALLBACK(configure_callback), &pb);

//	pm = gdk_pixmap_new(

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

static gint
configure_callback(GtkWidget *widget, GdkEventConfigure *event, GdkPixbuf **pb)
{
	printf("configure event occur\n");
	printf("width: %d\n", event->width);
	printf("height: %d\n", event->height);
	*pb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, event->width, event->height);
}

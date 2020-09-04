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

cairo_status_t
// read_png_file(char *fn, unsigned char *data, unsigned int length)
read_png_file(void *fp, unsigned char *data, unsigned int length)
{
	size_t r;

	printf("read_png_file: length = %d\n", length);

	r = fread(data, sizeof(unsigned char), length, fp);

	if (r < length) return CAIRO_STATUS_READ_ERROR;
	else return CAIRO_STATUS_SUCCESS;
}

int
main(int argc, char *argv[])
{
	GtkWidget *w, *da;
	cairo_surface_t *pngimage;
	FILE *fp;
	
	gtk_init(&argc, &argv);

	if (argc != 2) {
		printf ("tryPnfFromFile some.png\n");
		return 1; }

	if ((fp = fopen(argv[1], "r")) == NULL) {
		printf("error: no such file: %s\n", argv[1]);
		return CAIRO_STATUS_READ_ERROR;
	}

	pngimage = cairo_image_surface_create_from_png_stream(read_png_file, fp);

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

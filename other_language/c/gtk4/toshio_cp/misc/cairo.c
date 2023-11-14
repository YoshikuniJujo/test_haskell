#include <cairo.h>

int
main(int argc, char *argv[])
{
	cairo_surface_t *surface;
	cairo_t *cr;
	int width = 100;
	int height = 100;
	int square_size = 40.0;

	surface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, width, height);
	cr = cairo_create(surface);

	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_paint(cr);
	cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
	cairo_set_line_width(cr, 2.0);
	cairo_rectangle(cr,
		width / 2.0 - square_size / 2, height / 2.0 - square_size / 2,
		square_size, square_size);
	cairo_stroke(cr);

	cairo_surface_write_to_png(surface, "result/rectangle.png");
	cairo_destroy(cr);
	cairo_surface_destroy(surface);

	return 0;
}

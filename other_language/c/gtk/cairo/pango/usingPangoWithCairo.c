#include <stdio.h>
#include <gtk/gtk.h>

#define RADIUS 150
// #define FONT "Sans Bold 27"
#define FONT "Sans Bold"

static void
draw_text(cairo_t *cr)
{
	PangoLayout *layout;
	PangoFontDescription *desc;
	int i;

	PangoRectangle ink_rect, logical_rect;

	printf("PANGO_SCALE: %d\n", PANGO_SCALE);

	cairo_translate(cr, RADIUS, RADIUS);

	layout = pango_cairo_create_layout(cr);

	pango_layout_set_text (layout, "Text", -1);
	desc = pango_font_description_from_string (FONT);
	pango_font_description_set_size(desc, 27 * PANGO_SCALE);
	pango_layout_set_font_description(layout, desc);

	pango_layout_get_extents(layout, &ink_rect, &logical_rect);
	printf("%d %d %d %d\n", ink_rect.x, ink_rect.y, ink_rect.width, ink_rect.height);
	printf("%d %d %d %d\n", logical_rect.x, logical_rect.y, logical_rect.width, logical_rect.height);
	pango_layout_get_pixel_extents(layout, &ink_rect, &logical_rect);
	printf("%d %d %d %d\n", ink_rect.x, ink_rect.y, ink_rect.width, ink_rect.height);
	printf("%d %d %d %d\n", logical_rect.x, logical_rect.y, logical_rect.width, logical_rect.height);

	pango_cairo_show_layout(cr, layout);

	cairo_move_to (cr, 50, 50);
	pango_font_description_set_size(desc, 20 * PANGO_SCALE);
	pango_layout_set_font_description(layout, desc);

	pango_layout_get_extents(layout, &ink_rect, &logical_rect);
	printf("%d %d %d %d\n", ink_rect.x, ink_rect.y, ink_rect.width, ink_rect.height);
	printf("%d %d %d %d\n", logical_rect.x, logical_rect.y, logical_rect.width, logical_rect.height);
	pango_layout_get_pixel_extents(layout, &ink_rect, &logical_rect);
	printf("%d %d %d %d\n", ink_rect.x, ink_rect.y, ink_rect.width, ink_rect.height);
	printf("%d %d %d %d\n", logical_rect.x, logical_rect.y, logical_rect.width, logical_rect.height);

	pango_cairo_show_layout(cr, layout);

	pango_font_description_free(desc);
	g_object_unref(layout);
}

int
main(int argc, char *argv[])
{
	cairo_t *cr;
	char *filename;
	cairo_status_t status;
	cairo_surface_t *surface;

	filename = "some.png";

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 2 * RADIUS, 2 * RADIUS);
	cr = cairo_create(surface);

	cairo_move_to(cr, 100, 100);
	cairo_line_to (cr, 400, 400);
	cairo_stroke(cr);

	draw_text(cr);

	status = cairo_surface_write_to_png(surface, filename);
	cairo_surface_destroy(surface);

	if (status != CAIRO_STATUS_SUCCESS) {
		g_printerr("Could not save png to '%s'\n", filename);
		return 1; }

	return 0;
}

#include <gtk/gtk.h>
#include <math.h>
#include <time.h>

float m_radius = 0.42;
float m_line_width = 0.05;

static void
draw_clock(
	GtkDrawingArea *area, cairo_t *cr,
	int width, int height, gpointer user_data )
{

	cairo_scale(cr, width, height);
	cairo_translate(cr, 0.5, 0.5);

	cairo_set_line_width(cr, m_line_width);
	cairo_save(cr);

	cairo_set_source_rgba(cr, 0.337, 0.612, 0.117, 0.9);
	cairo_paint(cr);

	cairo_restore(cr);
	cairo_arc(cr, 0.0, 0.0, m_radius, 0.0, 2.0 * M_PI);
	cairo_save(cr);

	cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.8);
	cairo_fill_preserve(cr);

	cairo_restore(cr);
	cairo_stroke_preserve(cr);
	cairo_clip(cr);

	for (int i = 0; i < 12; i++) {
		double inset = 0.05;

		cairo_save(cr);
		cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);

		if (i % 3 != 0) {
			inset *= 0.8;
			cairo_set_line_width(cr, 0.03);
		}

		cairo_move_to(
			cr,
			(m_radius - inset) * cos(i * M_PI / 6.0),
			(m_radius - inset) * sin(i * M_PI / 6.0));
		cairo_line_to(
			cr,
			m_radius * cos(i * M_PI / 6.0),
			m_radius * sin(i * M_PI / 6.0));
		cairo_stroke(cr);
		cairo_restore(cr);
	}

	time_t rawtime;
	time(&rawtime);
	struct tm *timeinfo = localtime(&rawtime);

	double hours = timeinfo->tm_hour * M_PI / 6.0;
	double minutes = timeinfo->tm_min * M_PI / 30.0;
	double seconds = timeinfo->tm_sec * M_PI / 30.0;

	cairo_save(cr);
	cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);

	cairo_save(cr);

	cairo_set_line_width(cr, m_line_width / 3.0);
	cairo_set_source_rgba(cr, 0.7, 0.7, 0.7, 0.8);
	cairo_move_to(cr, 0.0, 0.0);
	cairo_line_to(
		cr,
		sin(seconds) * (m_radius * 0.9),
		-cos(seconds) * (m_radius * 0.9));
	cairo_stroke(cr);
	cairo_restore(cr);

	cairo_set_source_rgba(cr, 0.117, 0.337, 0.612, 0.9);
	cairo_move_to(cr, 0, 0);
	cairo_line_to(cr,
		sin(minutes + seconds / 60) * (m_radius * 0.8),
		-cos(minutes + seconds / 60) * (m_radius * 0.8));
	cairo_stroke(cr);

	cairo_set_source_rgba(cr, 0.337, 0.612, 0.117, 0.9);
	cairo_move_to(cr, 0.0, 0.0);
	cairo_line_to(cr,
		sin(hours + minutes / 12.0) * (m_radius * 0.5),
		-cos(hours + minutes / 12.0) * (m_radius * 0.5));
	cairo_stroke(cr);
	cairo_restore(cr);

	cairo_arc(cr, 0.0, 0.0, m_line_width / 3.0, 0.0, 2.0 * M_PI);
	cairo_fill(cr);
}

gboolean
time_handler(GtkWidget *widget)
{
	gtk_widget_queue_draw(widget);

	return TRUE;
}

static void
app_activate(GApplication *app, gpointer user_data)
{
	GtkWidget *win;
	GtkWidget *clock;
	GtkBuilder *build;

	build = gtk_builder_new_from_resource("/com/github/ToshioCP/tfc/tfc.ui");
	win = GTK_WIDGET(gtk_builder_get_object(build, "win"));
	gtk_window_set_application(GTK_WINDOW(win), GTK_APPLICATION(app));

	clock = GTK_WIDGET(gtk_builder_get_object(build, "clock"));
	g_object_unref(build);

	gtk_drawing_area_set_draw_func(
		GTK_DRAWING_AREA(clock), draw_clock, NULL, NULL);
	g_timeout_add(1000, (GSourceFunc)time_handler, (gpointer)clock);
	gtk_window_present(GTK_WINDOW(win));
}

static void
app_open(
	GApplication *app, GFile **files,
	gint n_files, gchar *hint, gpointer user_data )
{
	app_activate(app, user_data);
}

int
main(int argc, char *argv[])
{
	GtkApplication *app;
	int stat;

	app = gtk_application_new(
		"com.github.ToshioCP.tfc", G_APPLICATION_HANDLES_OPEN );
	g_signal_connect(app, "activate", G_CALLBACK(app_activate), NULL);
	g_signal_connect(app, "open", G_CALLBACK(app_open), NULL);
	stat = g_application_run(G_APPLICATION(app), argc, argv);
	g_object_unref(app);

	return stat;
}

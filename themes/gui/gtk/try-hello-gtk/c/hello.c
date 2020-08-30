#include <gtk/gtk.h>

void
foo(int *n)
{
	printf("%d\n", *n);
	(*n)++;
}

void
foo_array(int *n, char ***strs)
{
	for (int i = 0; i < *n; i++) printf("%s\n", (*strs)[i]);
}

int
// hello_main(int argc, char *argv[])
hello_main()
{
	GtkWidget *window;

	int argc = 0;
	char **argv;

//	gtk_init(&argc, &argv);
	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "ハロー");

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}

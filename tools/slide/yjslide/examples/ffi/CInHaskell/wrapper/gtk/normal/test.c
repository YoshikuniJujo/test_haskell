#include <gtk/gtk.h>
#include <stdio.h>

static void button_clicked(GtkWidget *button, gpointer user_data) {
	printf("button clicked!\n");
}

static void quit_clicked(GtkWidget *button, gpointer user_data) {
	gtk_main_quit();
}

int main(int argc, char *argv[]) {
	GtkWidget *window;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_size_request(window, 300, 200);

	{
		GtkWidget *vbox;
		vbox = gtk_vbox_new(FALSE, 2);
		gtk_container_add(GTK_CONTAINER(window), vbox);

		GtkWidget *button;
		button = gtk_button_new_with_label("Button");
		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		g_signal_connect(G_OBJECT(button), "clicked",
			G_CALLBACK(button_clicked), NULL);

		GtkWidget *quit;
		quit = gtk_button_new_with_label("Quit");
		gtk_box_pack_start(GTK_BOX(vbox), quit, FALSE, FALSE, 0);
		g_signal_connect(G_OBJECT(quit), "clicked",
			G_CALLBACK(quit_clicked), NULL);
	}

	g_signal_connect(window, "destroy",
		G_CALLBACK(gtk_main_quit), NULL);

	gtk_widget_show_all(window);

	gtk_main();

	return 0;
}

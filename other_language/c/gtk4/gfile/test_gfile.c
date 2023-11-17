#include <gtk/gtk.h>

#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

gpointer
sleep_some(GCancellable *cancellable)
{
	FILE *fp = fopen("fifo", "w");

	g_usleep(1000000);

	g_print("Sleeped\n");

//	int file = open("fifo", O_WRONLY);

	fprintf(fp, "foo\n");
	fflush(fp);

	g_usleep(1000000);
	fprintf(fp, "bar\n");
	fflush(fp);
//	g_usleep(1000000);

	g_cancellable_cancel(cancellable);

	fclose(fp);

	return NULL;
}

int
main(int argc, char *argv[])
{
	GFile *file;
	GCancellable *cancellable;
	char *contents;
	gsize length;
	GError *err = NULL;

	cancellable = g_cancellable_new();

	g_thread_new(NULL, (GThreadFunc) sleep_some, (void *) cancellable);

//	file = g_file_new_for_path("hello.txt");
	file = g_file_new_for_path(argv[1]);

	printf("file created\n");

	gboolean r = g_file_load_contents(file, cancellable, &contents, &length, NULL, &err);

	printf("%s", contents);

	if (r == FALSE) {
		g_print("%d %d %s\n", err->domain, err->code, err->message);
		g_print("%d\n", G_IO_ERROR_CANCELLED);
	}

	return 0;
}

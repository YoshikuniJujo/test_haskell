#include <stdio.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	sqlite3 *db = NULL;
	int ret = 0;

	ret = sqlite3_open("hello.sqlite3", &db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	printf("sqlite3 *db = %p\n", db);

	ret = sqlite3_close(db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot close database: %s\n", sqlite3_errmsg(db));
		return 1; }
	db = NULL;

	return 0;
}

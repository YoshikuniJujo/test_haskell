#include <stdio.h>
#include <sqlite3.h>

int
show_row(void *v0, int n, char **vs, char **ns)
{
	int i;

	for (i = 0; i < n; i++) {
		printf("%s: %s", ns[i], vs[i]);
		if (i == n - 1) break;
		printf(", "); }
	printf("\n");

	return 0;
}

int
main(int argc, char *argv[])
{
	sqlite3 *db = NULL;
	int ret = 0;
	char *msg = NULL;

	ret = sqlite3_open("hello.sqlite3", &db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_exec(db,
		"CREATE TABLE greeting(words, greetee)", NULL, NULL, &msg);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot create table: %s\n", msg);
		sqlite3_close(db);
		return 1; }
	sqlite3_free(msg);

	ret = sqlite3_exec(db,
		"INSERT INTO greeting VALUES ('hello', 'world')", NULL, NULL, &msg);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot insert data: %s\n", msg);
		sqlite3_close(db);
		return 1; }
	sqlite3_free(msg);

	ret = sqlite3_exec(db,
		"INSERT INTO greeting VALUES ('good-bye', 'world')", NULL, NULL, &msg);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot insert data: %s\n", msg);
		sqlite3_close(db);
		return 1; }
	sqlite3_free(msg);

	ret = sqlite3_exec(db,
		"INSERT INTO greeting VALUES ('hello', 'you')", NULL, NULL, &msg);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot insert data: %s\n", msg);
		sqlite3_close(db);
		return 1; }
	sqlite3_free(msg);

	ret = sqlite3_exec(db, "SELECT * FROM greeting", show_row, NULL, &msg);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot display data: %s\n", msg);
		sqlite3_close(db);
		return 1; }
	sqlite3_free(msg);

	ret = sqlite3_close(db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot close database: %s\n", sqlite3_errmsg(db));
		return 1; }
	db = NULL;

	return 0;
}

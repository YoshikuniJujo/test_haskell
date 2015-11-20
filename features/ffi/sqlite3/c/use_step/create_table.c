#include <stdio.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	sqlite3 *db = NULL;
	sqlite3_stmt *stmt = NULL;
	const char *tail = NULL;
	int ret = 0;

	ret = sqlite3_open("hello.sqlite3", &db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_prepare_v2(db,
		"CREATE TABLE greeting(id, words, greetee)", -1, &stmt, &tail);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot prepare stmt: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_step(stmt);
	if (ret != SQLITE_DONE) {
		fprintf(stderr, "Cannot create table: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_finalize(stmt);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot finalize stmt: %s\n", sqlite3_errmsg(db));
		return 1; }

	ret = sqlite3_close(db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot close database: %s\n", sqlite3_errmsg(db));
		return 1; }
	db = NULL;

	return 0;
}

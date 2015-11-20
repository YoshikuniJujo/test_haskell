#include <stdio.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	sqlite3 *db = NULL;
	sqlite3_stmt *stmt = NULL;
	const char *tail = NULL;
	int ret = 0;
	int idx = 0;
	char *values[][2] = {
		{ "hello", "world" },
		{ "good-bye", "world" },
		{ "hello", "you" } };
	int i, j, k = 0;

	ret = sqlite3_open("hello.sqlite3", &db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_prepare_v2(db,
		"INSERT INTO greeting VALUES (:id, :words, :greetee)",
		-1, &stmt, &tail);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot prepare stmt: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	for (i = 0; i < 3; i++) {
		for (j = 0; j < 3; j ++) {
			idx = sqlite3_bind_parameter_index(stmt, ":id");
			sqlite3_bind_int(stmt, idx, ++k);
			idx = sqlite3_bind_parameter_index(stmt, ":words");
			sqlite3_bind_text(
				stmt, idx, values[j][0], -1, SQLITE_STATIC);
			idx = sqlite3_bind_parameter_index(stmt, ":greetee");
			sqlite3_bind_text(
				stmt, idx, values[j][1], -1, SQLITE_STATIC);
			ret = sqlite3_step(stmt);
			if (ret != SQLITE_DONE) {
				fprintf(stderr,
					"Cannot create table: %s\n",
					sqlite3_errmsg(db));
				sqlite3_close(db);
				return 1; }
			sqlite3_reset(stmt);
		}
	}


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

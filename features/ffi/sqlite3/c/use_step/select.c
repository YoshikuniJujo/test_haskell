#include <stdio.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	sqlite3 *db = NULL;
	sqlite3_stmt *stmt = NULL;
	const char *tail = NULL;
	int ret = 0;
	int i;

	ret = sqlite3_open("hello.sqlite3", &db);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	ret = sqlite3_prepare_v2(db,
		"SELECT * FROM greeting", -1, &stmt, &tail);
	if (ret != SQLITE_OK) {
		fprintf(stderr, "Cannot prepare stmt: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	i = 0;
	ret = SQLITE_ROW;
	while (ret == SQLITE_ROW) {
		ret = sqlite3_step(stmt);
		if (ret != SQLITE_ROW && ret != SQLITE_DONE) {
			fprintf(stderr, "Cannot select: %d, %s\n", ret, sqlite3_errmsg(db));
			sqlite3_finalize(stmt);
			sqlite3_close(db);
			return 1; }
		if (ret == SQLITE_ROW) {
			printf("%d|%s|%s\n",
				sqlite3_column_int(stmt, 0),
				sqlite3_column_text(stmt, 1),
				sqlite3_column_text(stmt, 2));
		}
		if (++i >= 5) break;
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

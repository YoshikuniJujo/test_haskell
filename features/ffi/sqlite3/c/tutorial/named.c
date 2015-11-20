#include <sqlite3.h>
#include <stdio.h>

int
main(void)
{
	sqlite3 *db;
	char *err_msg = 0;
	sqlite3_stmt *res;

	int rc = sqlite3_open("test.db", &db);

	if (rc != SQLITE_OK) {
		fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
		sqlite3_close(db);
		return 1; }

	char *sql = "SELECT Id, Name FROM Cars WHERE Id = @id";

	rc = sqlite3_prepare_v2(db, sql, -1, &res, 0);

	if (rc == SQLITE_OK) {
		int idx = sqlite3_bind_parameter_index(res, "@id");
		int value = 4;
		sqlite3_bind_int(res, idx, value);
	} else {
		fprintf(stderr, "Failed to execute statement: %s\n", sqlite3_errmsg(db));
	}

	int step = sqlite3_step(res);

	if (step == SQLITE_ROW) {
		printf("%s: ", sqlite3_column_text(res, 0));
		printf("%s\n", sqlite3_column_text(res, 1));
	}

	sqlite3_finalize(res);
	sqlite3_close(db);

	return 0;
}

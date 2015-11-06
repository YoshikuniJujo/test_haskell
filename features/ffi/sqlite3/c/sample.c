#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sqlite3.h>

sqlite3 *
test_open(void)
{
	int ret = 0;
	sqlite3 *conn = NULL;
	ret = sqlite3_open("sample.sqlite3", &conn);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot open sample.sqlite3");
		exit(-1); }
	return conn;
}

void
test_close(sqlite3 *conn)
{
	int ret = 0;
	ret = sqlite3_close(conn);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot close");
		exit(-1); }
}

int
main(int argc, char *argv[])
{
	sqlite3 *conn = NULL;
	int ret = 0;
	char *err_msg = NULL;
	sqlite3_stmt *stmt = NULL;

	conn = test_open();

	ret = sqlite3_exec(conn,
		"CREATE TABLE IF NOT EXISTS test "
		"(id INTEGER PRIMARY KEY AUTOINCREMENT, "
		"name CHAR(32) NOT NULL)",
		NULL, NULL, &err_msg);
	if (ret != SQLITE_OK) {
		printf("%s\n", err_msg);
		sqlite3_free(err_msg);
		exit(-1); }

	ret = sqlite3_prepare_v2(conn,
		"INSERT INTO test (name) VALUES (?)",
		128, &stmt, NULL);
	if (ret != SQLITE_OK) { exit(-1); }

	int i = 0;
	for (i = 0; i < 5; i++) {
		char name[32];
		snprintf(name, 32, "test%02d", i);
		sqlite3_bind_text(stmt, 1, name, strlen(name), SQLITE_STATIC);
		while(SQLITE_DONE != sqlite3_step(stmt)) {}
	}

	sqlite3_finalize(stmt);

	ret = sqlite3_prepare_v2(conn,
		"SELECT * FROM test", 64,
		&stmt, NULL);
	if (ret != SQLITE_OK) { exit(-1); }

	while (SQLITE_ROW == (ret = sqlite3_step(stmt))) {
		int id = sqlite3_column_int(stmt, 0);
		const unsigned char *name = sqlite3_column_text(stmt, 1);
		printf("id: %d, name: %s\n", id, name);
	}

	if (ret != SQLITE_DONE) { exit(-1); }

	sqlite3_finalize(stmt);

	test_close(conn);

	return 0;
}

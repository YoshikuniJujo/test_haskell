#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

sqlite3 *
test_open(void)
{
	int ret = 0;

	sqlite3 *conn = NULL;
	ret = sqlite3_open("hello.sqlite3", &conn);
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

void
sample_table(sqlite3 *conn)
{
	int ret = 0;
	char *err_msg = NULL;

	ret = sqlite3_exec(conn,
		"CREATE TABLE IF NOT EXISTS test "
		"(id INTEGER PRIMARY KEY AUTOINCREMENT, "
		"name CHAR(32) NOT NULL)",
		NULL, NULL, &err_msg);
	if (ret != SQLITE_OK) {
		printf("%s\n", err_msg);
		sqlite3_free(err_msg);
		test_close(conn);
		exit(-1);
		}
}

sqlite3_stmt *
insert_stmt(sqlite3 *conn)
{
	int ret = 0;
	sqlite3_stmt *stmt = NULL;

	ret = sqlite3_prepare_v2(conn,
		"INSERT INTO test (name) VALUES (?)", 128, &stmt, NULL);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot make insert stmt");
		exit(-1);
		}

	return stmt;
}

void
insert(sqlite3 *conn, sqlite3_stmt *stmt, char *name)
{
	sqlite3_bind_text(stmt, 1, name, strlen(name), SQLITE_STATIC);
	while (SQLITE_DONE != sqlite3_step(stmt)) {}
}

sqlite3_stmt *
select_stmt(sqlite3 *conn)
{
	int ret = 0;
	sqlite3_stmt *stmt = NULL;

	ret = sqlite3_prepare_v2(conn,
		"SELECT * FROM test WHERE id = ?",
		64, &stmt, NULL);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot make select stmt");
		exit(-1); }

	return stmt;
}

void
sql_select(sqlite3 *conn, sqlite3_stmt *stmt, int id, int n, char *str)
{
	int ret = 0;
	const char *name = NULL;

	sqlite3_bind_int(stmt, 1, id);
	while (SQLITE_ROW == (ret = sqlite3_step(stmt))) {
		name = sqlite3_column_text(stmt, 1);
//		printf("debug: %s\n", name);
		strncpy(str, name, n);
	}
	if (ret != SQLITE_DONE) {
		printf("SQLITE3: error while selection");
		exit(-1); }
}

int
count(sqlite3 *conn)
{
	int c = 0;
	sqlite3_stmt *stmt = NULL;

	sqlite3_prepare_v2(conn, "SELECT * FROM test", 128, &stmt, NULL);
	while (SQLITE_DONE != sqlite3_step(stmt)) { c++; }
	sqlite3_finalize(stmt);

	return c;
}

int
exist(sqlite3 *conn, int id)
{
	int c = 0;
	sqlite3_stmt *stmt = NULL;

	sqlite3_prepare_v2(conn,
		"SELECT * FROM test WHERE id = ?",
		128, &stmt, NULL);
	sqlite3_bind_int(stmt, 1, id);
	if (SQLITE_DONE != sqlite3_step(stmt)) { c++; }
	sqlite3_finalize(stmt);

	return c;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>

sqlite3 *
sql_open(void)
{
	int ret = 0;
	sqlite3 *conn = NULL;

	ret = sqlite3_open("accounts.sqlite3", &conn);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot open accounts.sqlite3\n");
		exit(-1); }
	return conn;
}

void
sql_close(sqlite3 *conn)
{
	int ret = 0;

	ret = sqlite3_close(conn);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot close\n");
		exit(-1); }
}

sqlite3_stmt *
mk_stmt(sqlite3 *conn, char *str)
{
	int ret = 0;
	sqlite3_stmt *stmt = NULL;

	ret = sqlite3_prepare_v2(conn, str, 256, &stmt, NULL);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: cannot make stmt: %s\n", str);
		exit(-1);
		}

	return stmt;
}

void
bind_stmt(sqlite3_stmt *stmt, char *ph, char *val)
{
	sqlite3_bind_text(stmt, sqlite3_bind_parameter_index(stmt, ph),
		val, strlen(val), SQLITE_TRANSIENT);
}

void
run_stmt(sqlite3_stmt *stmt)
{
	int ret;

	while (SQLITE_DONE != (ret = sqlite3_step(stmt))) {
		if (ret != SQLITE_OK && ret != SQLITE_ROW) break; }
	if (ret != SQLITE_DONE) {
		printf("SQLITE3: error run_stmt: error code is %d\n", ret);
		exit(-1); }
}

int
get_stmt(sqlite3_stmt *stmt, int n, char *str)
{
	int ret = 0;
	int c = 0;
	const char *val = NULL;

	while (SQLITE_ROW == (ret = sqlite3_step(stmt))) {
		c++;
		val = sqlite3_column_text(stmt, 0);
		strncpy(str, val, n); }
	if (ret != SQLITE_DONE) {
		printf("SQLITE3: error while get");
		exit(-1); }
	return c;
}

void
get2_stmt(sqlite3_stmt *stmt, int n, char *str1, char *str2)
{
	int ret = 0;
	const char *val = NULL;

	while (SQLITE_ROW == (ret = sqlite3_step(stmt))) {
		val = sqlite3_column_text(stmt, 0);
		strncpy(str1, val, n);
		val = sqlite3_column_text(stmt, 1);
		strncpy(str2, val, n); }
	if (ret != SQLITE_DONE) {
		printf("SQLITE3: error while get");
		exit(-1); }
}

int
exist_stmt(sqlite3_stmt *stmt)
{
	int c = 0;

	if (SQLITE_DONE != sqlite3_step(stmt)) { c++; }

	return c;
}

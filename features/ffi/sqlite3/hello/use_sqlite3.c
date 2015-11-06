#include <stdio.h>
#include <stdlib.h>
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

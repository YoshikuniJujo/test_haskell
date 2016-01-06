#include <stdio.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	printf("SQLITE_READONLY = %d\n", SQLITE_READONLY);
	printf("SQLITE_READONLY_DBMOVED = %d\n\n", SQLITE_READONLY_DBMOVED);

	printf("SQLITE_AUTH = %d\n", SQLITE_AUTH);
	printf("SQLITE_AUTH_USER = %d\n", SQLITE_AUTH_USER);

	return 0;
}

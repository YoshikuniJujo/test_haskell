sqlite3 *sql_open(void);
void sql_close(sqlite3 *conn);
sqlite3_stmt *mk_stmt(sqlite3 *conn, char *str);
void bind_stmt(sqlite3_stmt *stmt, char *ph, char *val);
void run_stmt(sqlite3_stmt *stmt);
void get_stmt(sqlite3_stmt *stmt, int n, char *str);
void get2_stmt(sqlite3_stmt *stmt, int n, char *str1, char *str2);
int exist_stmt(sqlite3_stmt *stmt);

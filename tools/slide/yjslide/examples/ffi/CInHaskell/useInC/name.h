typedef struct name {
	char first_name[20];
	char last_name[20];
} name;

name* mkName(char *, char *);
void printName(name *);
void freeName(name *);

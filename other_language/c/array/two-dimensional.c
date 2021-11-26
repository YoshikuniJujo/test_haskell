#include <stdlib.h>
#include <stdio.h>

typedef char FieldArray[23][80];

typedef char (*Field)[80];

Field
field_new(void)
{
	Field f;

	f = (Field)malloc(sizeof(FieldArray));
	for (int i = 0; i < 23; i++) {
		int j;
		for (j = 0; j < 79; j++) {
			f[i][j] = '.';
		}
		f[i][j] = '\0';
	}

	return f;
}

void
field_draw(Field f)
{
	for (int i = 0; i < 23; i++) {
		printf("%s\n", f[i]);
	}
}

int
main(int argc, char *argv[])
{
	Field f = field_new();
	field_draw(f);

	printf("%ld\n", sizeof(FieldArray));

	return 0;
}

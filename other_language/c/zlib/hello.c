#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

int
main(int argc, char *argv[])
{
	printf("zlib version: %s\n", ZLIB_VERSION);
	printf("zlib vernum : %d\n", ZLIB_VERNUM);
	printf("argc        : %d\n", argc);

	for (int i = 0; i < argc; i++)
		printf("argv[%d]: %s\n", i, argv[i]);

	if (argc < 2) {
		printf("need .gz file path\n");
		exit(1); }

	gzFile inf = gzopen(argv[1], "rb");

	if (inf == NULL) {
		printf("cannot open such file: %s\n", argv[1]);
		exit(1); }

	char buf[64];

	gzgets(inf, buf, 64);
	printf("1st line: %s", buf);

	gzclose(inf);

	return 0;
}

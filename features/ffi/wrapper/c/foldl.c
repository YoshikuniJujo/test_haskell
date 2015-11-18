typedef int (*Op)(int x, int y);

int
foldl(Op op, int x0, int n, int xs[])
{
	int s = x0; int i = 0;

	for (i = 0; i < n; i ++) s = op(s, xs[i]);
	return s;
}

int add(int x, int y) { return x + y; }
int mul(int x, int y) { return x * y; }

int sample[] = { 3, 5, 8, 9 };

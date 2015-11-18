typedef int (*Op)(int x, int y);

int foldl(Op op, int x0, int n, int xs[]);

int add(int x, int y);
int mul(int x, int y);

int sample[];

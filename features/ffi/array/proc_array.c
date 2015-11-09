void
proc_array(int n, int max, int ns[])
{
	int c = 0;
	int *t = ns;

	for (c = 0; c < n; c ++) {
		if (c >= max) break;
		*(t++) = c;
		}
	*t = -1;
}

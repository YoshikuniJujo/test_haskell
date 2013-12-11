int *counter(void) {
	static int c = 0;
	c++;
	return &c;
}

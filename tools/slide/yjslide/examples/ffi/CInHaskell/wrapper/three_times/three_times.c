void three_times(void (*f)(void)) {
	int i;
	for (i = 0; i < 3; i++) f();
}

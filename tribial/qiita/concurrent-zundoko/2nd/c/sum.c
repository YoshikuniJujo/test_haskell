#include <stdio.h>

int sum (int *nums);

int
main(int argc, char *argv[])
{
	int nums[] = { 123, 456, 789, -1 };
	printf("%d\n", sum(nums));
	return 0;
}

int
sum(int *nums)
{
	int s = 0;
	for (int i = 0; nums[i] >= 0; i++) {
		s += nums[i]; }
	return s;
}

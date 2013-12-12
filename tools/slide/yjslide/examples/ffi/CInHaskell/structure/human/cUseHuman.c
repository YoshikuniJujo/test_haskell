#include <stdio.h>
#include "human.h"

int main(int argc, char *argv[]) {
	human *t = get_tarou();
	printf("太郎: %d歳 %3.1fcm %2.1fkg\n", t->age, t->height, t->weight);
}

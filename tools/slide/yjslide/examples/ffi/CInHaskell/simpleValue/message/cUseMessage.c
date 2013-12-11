#include <stdlib.h>
#include <stdio.h>
#include "message.h"

int main() {
	enum Message *msg = calloc(1, sizeof(enum Message));
	sellect_message(msg);
	*msg = GoodNight;
	message();
	*msg = Hi;
	message();
	free(msg);
}

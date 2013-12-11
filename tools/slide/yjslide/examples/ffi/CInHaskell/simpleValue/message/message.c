#include <stdlib.h>
#include <stdio.h>
#include "message.h"

char *message_list[] = {
	"Hello, world!",
	"Good-bye, world!",
	"Good night, world!",
	"Hi, world!"
};

static Message *message_type;

void sellect_message(Message *n) { message_type = n; }

void message(void) {
	printf("%s\n", message_list[*message_type]); }

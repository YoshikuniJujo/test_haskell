#include "human.h"

static human tarou = { 35, 182.5, 75 };

human *get_tarou(void) {
	return &tarou;
}

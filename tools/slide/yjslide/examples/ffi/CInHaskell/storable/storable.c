#include <stdio.h>
#include "storable.h"

void printMaybeDouble(MaybeDouble *ptr) {
	if (ptr->isJust) {
		printf("Just %f\n", ptr->value);
	} else {
		printf("Nothing\n");
	}
}

void printEitherShortDouble(EitherShortDouble *ptr) {
	if (ptr->isRight) {
		printf("Right %f\n", ptr->value.right);
	} else {
		printf("Left %d\n", ptr->value.left);
	}
}

void printShape(Shape *ptr) {
	switch (ptr->shape_type) {
	case 0:
		printf("Circle %f\n", ptr->shape_value.radius);
		break;
	case 1:
		printf("Rectangle %f %f\n",
			ptr->shape_value.width_height.width,
			ptr->shape_value.width_height.height);
		break;
	default:
		printf("No such shape\n");
	}
}

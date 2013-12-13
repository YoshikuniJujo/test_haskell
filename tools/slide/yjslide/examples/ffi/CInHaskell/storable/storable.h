typedef struct MaybeDouble {
	int isJust;
	double value;
} MaybeDouble;

typedef struct EitherShortDouble {
	int isRight;
	union _value {
		short left;
		double right; } value;
} EitherShortDouble;

typedef struct Shape {
	int shape_type;
	union _shapes {
		double radius;
		struct _rectangle {
			double width;
			double height;
		} width_height;
	} shape_value;
} Shape;

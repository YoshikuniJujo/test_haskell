#include <complex.h>

int escape_time(float complex c, int lm)
{
	float complex z = 0 + 0i;

	for (int i = 0; i <= lm; i++) {
		z = z * z + c;
		if (cabs(z) > 2) {
			return i;
		}
	}

	return -1;
}

float complex make_complex(float re, float im)
{
	return re + im * I;
}

int escape_time_hs(float re, float im, int lm)
{
	return escape_time(make_complex(re, im), lm);
}

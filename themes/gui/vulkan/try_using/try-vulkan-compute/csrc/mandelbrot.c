#include <stdint.h>
#include <complex.h>

int
escape_time(float complex c, int lm)
{
	float complex z = 0 + 0i;

	for (int i = 0; i <= lm; i++) {
		z = z * z + c;
		if (cabsf(z) > 2) {
			return i;
		}
	}

	return -1;
}

float complex
make_complex(float re, float im)
{
	return re + im * I;
}

int
escape_time_hs(float re, float im, int lm)
{
	return escape_time(make_complex(re, im), lm);
}

float complex
pixel_to_point(
	uint32_t w, uint32_t h, uint32_t x, uint32_t y,
	float complex upper_left, float complex lower_right )
{

	float width = crealf(lower_right) - crealf(upper_left);
	float height = cimagf(upper_left) - cimagf(lower_right);

	return crealf(upper_left) + (float)x * width / (float)w +
		(cimagf(upper_left) - (float)y * height / (float)h) * I;

}

void
get_complex(float complex c, float *re, float *im)
{
	*re = crealf(c); *im = cimagf(c);
}

void
pixel_to_point_hs(
	uint32_t w, uint32_t h, uint32_t x, uint32_t y,
	float lft, float upr, float rgt, float lwr, float *re, float *im )
{
	float complex upper_left = make_complex(lft, upr);
	float complex lower_right = make_complex(rgt, lwr);
	float complex c = pixel_to_point(w, h, x, y, upper_left, lower_right);
	get_complex(c, re, im);
}

void
render(uint32_t pixels[],
	uint32_t w, uint32_t h, float complex upper_left, float complex lower_right)
{
	for (uint32_t row = 0; row < h; row++)
		for (uint32_t column = 0; column < w; column++) {
			float complex point = pixel_to_point(
				w, h, column, row, upper_left, lower_right );
			int t = escape_time(point, 255);
			if (t < 0) pixels[row * w + column] = 0;
			else pixels[row * w + column] = 255 - t;
		}
}

void
render_hs(uint32_t pixels[],
	uint32_t w, uint32_t h, float lft, float upr, float rgt, float lwr)
{
	float complex upper_left = lft + upr * I;
	float complex lower_right = rgt + lwr * I;
	render(pixels, w, h, upper_left, lower_right);
}

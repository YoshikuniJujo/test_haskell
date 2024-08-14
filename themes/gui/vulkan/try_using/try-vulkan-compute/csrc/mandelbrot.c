#include <stdint.h>
#include <complex.h>

int
escape_time(float complex c, int lm)
{
	float complex z = 0 + 0i;

	for (int i = 0; i <= lm; i++) {
		z = z * z + c;
		if (cabsf(z) > 2) { return i; } }

	return -1;
}

float complex
pixel_to_point(
	uint32_t pxw, uint32_t pxh,
	float complex tl, float complex rb, uint32_t x, uint32_t y )
{
	float pntw = crealf(rb) - crealf(tl);
	float pnth = cimagf(tl) - cimagf(rb);

	return crealf(tl) + (float)x * pntw / (float)pxw +
		(cimagf(tl) - (float)y * pnth / (float)pxh) * I;
}

void
render(uint32_t pxs[],
	uint32_t w, uint32_t h, float l, float t, float r, float b)
{
	float complex tl = l + t * I; float complex rb = r + b * I;

	for (uint32_t y = 0; y < h; y++)
	for (uint32_t x = 0; x < w; x++) {
		float complex pnt = pixel_to_point(w, h, tl, rb, x, y);
		int t = escape_time(pnt, 255);
		if (t < 0) pxs[y * w + x] = 0;
		else pxs[y * w + x] = 255 - t; }
}

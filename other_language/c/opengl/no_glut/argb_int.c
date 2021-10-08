#include <stdlib.h>
#include <stdio.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/glx.h>

/* Image size */
#define FBOWIDTH 64
#define FBOHEIGHT 32

GLubyte image[FBOHEIGHT][FBOWIDTH][4];

static GLuint fb;
static GLuint cb;
static GLuint rb;

static void
init(void)
{
	glClearColor(0.1, 0.4, 0.05, 1.0);

	// TEXTURE
	glGenTextures(1, &cb);
	glBindTexture(GL_TEXTURE_2D, cb);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FBOWIDTH, FBOHEIGHT, 0, GL_RGBA,
		GL_UNSIGNED_BYTE, 0);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	// RENDER BUFFER
	glGenRenderbuffers(1, &rb);
	glBindRenderbuffer(GL_RENDERBUFFER, rb);
	glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT,
		FBOWIDTH, FBOHEIGHT);
	glBindRenderbuffer(GL_RENDERBUFFER, 0);

	glGenFramebuffers(1, &fb);
	glBindFramebuffer(GL_FRAMEBUFFER, fb);

	// FRAME BUFFER
	glFramebufferTexture2D(GL_FRAMEBUFFER,
		GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, cb, 0);

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

static void
make_texture(double sz)
{
	glViewport(0, 0, FBOWIDTH, FBOHEIGHT);
	glBindFramebuffer(GL_FRAMEBUFFER, fb);

	// BIG SQUARE
	glClear(GL_COLOR_BUFFER_BIT);
	glColor3d(1.0, 0.2, 0.2);
	glBegin(GL_LINE_LOOP);
	glVertex2d(-0.9, -0.9);
	glVertex2d(0.9, -0.9);
	glVertex2d(0.9, 0.9);
	glVertex2d(-0.9, 0.9);
	glEnd();

	// SMALL SQUARE
	glColor3d(0.2, 0.2, 1.0);
	glBegin(GL_LINE_LOOP);
	glVertex2d(0.0, 0.0);
	glVertex2d(sz, 0.0);
	glVertex2d(sz, sz);
	glVertex2d(0.0, sz);
	glEnd();
	glFlush();

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

static void
make_context(Display *dpy, GLXContext *ctxRet)
{
	GLXContext ctx;
	XVisualInfo *visinfo;
	int attrs[] = { None };

	visinfo = glXChooseVisual(dpy, DefaultScreen(dpy), attrs);
	if (!visinfo) {
		printf("Error: couldn't get an RGB, Double-buffered visual\n");
		exit(1); }
	ctx = glXCreateContext( dpy, visinfo, NULL, True );
	if (!ctx) {
		printf("Error: glXCreateContext failed\n"); exit(1); }

	XFree(visinfo);
	*ctxRet = ctx;
}

void
dot(GLubyte px[])
{
	int n = (int)px[0] + (int)px[1] + (int)px[2];
	if (n < 0x80) printf(" ");
	else if (n < 0x100) printf(".");
	else if (n < 0x180) printf("+");
	else if (n < 0x200) printf("*");
	else printf("#");
}

void
output_image(GLubyte img[FBOHEIGHT][FBOWIDTH][4])
{
	FILE *fp = fopen("argb_int.raw", "w");
	fwrite(img, 4, FBOWIDTH * FBOHEIGHT, fp);
	fclose(fp);

	for (int i = 0; i < FBOHEIGHT; i++) {
		for (int j = 0; j < FBOWIDTH; j ++) {
			dot(img[i][j]); }
		printf("\n"); }
}

int
main(int argc, char *argv[])
{
	Display *dpy;
	GLXContext ctx;
	double size = 0.8;

	if (argc == 2) {
		double s = atof(argv[1]);
		if (0.1 <= s && s < 0.8) { size = s; } }

	dpy = XOpenDisplay(NULL);
	if (!dpy) {
		printf("Error: couldn't open display %s\n", getenv("DISPLAY"));
		return -1; }
	make_context(dpy, &ctx);
	glXMakeCurrent(dpy, None, ctx);
	init();
	make_texture(size);
	glGetTextureImage(
		cb, 0, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV,
		FBOWIDTH * FBOHEIGHT * 4, image );
	glXMakeCurrent(dpy, None, NULL);
	glXDestroyContext(dpy, ctx);
	XCloseDisplay(dpy);

	output_image(image);
	return 0;
}

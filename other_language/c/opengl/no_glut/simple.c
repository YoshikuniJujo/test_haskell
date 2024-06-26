#include <stdlib.h>
#include <stdio.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/glx.h>

/* Event handler results: */
#define NOP 0
#define EXIT 1
#define DRAW 2

/* Image size */
#define FBOWIDTH 64
#define FBOHEIGHT 32

GLubyte image[FBOHEIGHT][FBOWIDTH][4];

static GLuint fb;
static GLuint cb;
static GLuint rb;

int globalWidth, globalHeight;

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

	glClear(GL_COLOR_BUFFER_BIT);
	glBegin(GL_LINE_LOOP);
	glVertex2d(-0.9, -0.9);
	glVertex2d(0.9, -0.9);
	glVertex2d(0.9, 0.9);
	glVertex2d(-0.9, 0.9);
	glEnd();

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
display_texture(void)
{
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glViewport(0, 0, globalWidth, globalHeight);
	glDisable(GL_LIGHTING);
	glDisable(GL_DEPTH_TEST);
	glBindTexture(GL_TEXTURE_2D, cb);
	glEnable(GL_TEXTURE_2D);

	glColor3d(1.0, 1.0, 1.0);
	glBegin(GL_TRIANGLE_FAN);
	glTexCoord2d(0.0, 0.0);
	glVertex2d(-1.0, -1.0);
	glTexCoord2d(1.0, 0.0);
	glVertex2d(1.0, -1.0);
	glTexCoord2d(1.0, 1.0);
	glVertex2d(1.0, 1.0);
	glTexCoord2d(0.0, 1.0);
	glVertex2d(-1.0, 1.0);
	glEnd();

	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
}

/* new window size or exposure */
static void
reshape(int width, int height)
{
	globalWidth = width;
	globalHeight = height;
	glViewport(0, 0, (GLint) width, (GLint) height);
}

/*
 * Create an RGB, double-buffered window.
 * Return the window and context handles.
 */
static void
make_window( Display *dpy, const char *name,
             int x, int y, int width, int height,
             Window *winRet, GLXContext *ctxRet)
{
   int attribs[64];
   int i = 0;

   int scrnum;
   XSetWindowAttributes attr;
   unsigned long mask;
   Window root;
   Window win;
   GLXContext ctx;
   XVisualInfo *visinfo;

   /* Singleton attributes. */
   attribs[i++] = GLX_RGBA;
   attribs[i++] = GLX_DOUBLEBUFFER;

   /* Key/value attributes. */
   attribs[i++] = GLX_RED_SIZE;
   attribs[i++] = 1;
   attribs[i++] = GLX_GREEN_SIZE;
   attribs[i++] = 1;
   attribs[i++] = GLX_BLUE_SIZE;
   attribs[i++] = 1;
   attribs[i++] = GLX_DEPTH_SIZE;
   attribs[i++] = 1;

   attribs[i++] = None;

   scrnum = DefaultScreen( dpy );
   root = RootWindow( dpy, scrnum );

   visinfo = glXChooseVisual(dpy, scrnum, attribs);
   if (!visinfo) {
      printf("Error: couldn't get an RGB, Double-buffered");
      printf(" visual\n");
      exit(1);
   }

   /* window attributes */
   attr.background_pixel = 0;
   attr.border_pixel = 0;
   attr.colormap = XCreateColormap( dpy, root, visinfo->visual, AllocNone);
   attr.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask;
   /* XXX this is a bad way to get a borderless window! */
   mask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask;

   win = XCreateWindow( dpy, root, x, y, width, height,
		        0, visinfo->depth, InputOutput,
		        visinfo->visual, mask, &attr );

   /* set hints and properties */
   {
      XSizeHints sizehints;
      sizehints.x = x;
      sizehints.y = y;
      sizehints.width  = width;
      sizehints.height = height;
      sizehints.flags = USSize | USPosition;
      XSetNormalHints(dpy, win, &sizehints);
      XSetStandardProperties(dpy, win, name, name,
                              None, (char **)NULL, 0, &sizehints);
   }

   ctx = glXCreateContext( dpy, visinfo, NULL, True );
   if (!ctx) {
      printf("Error: glXCreateContext failed\n");
      exit(1);
   }

   XFree(visinfo);

   *winRet = win;
   *ctxRet = ctx;
}

/**
 * Handle one X event.
 * \return NOP, EXIT or DRAW
 */
static int
handle_event(Display *dpy, Window win, XEvent *event)
{
   (void) dpy;
   (void) win;

   switch (event->type) {
   case Expose:
      return DRAW;
   case ConfigureNotify:
      reshape(event->xconfigure.width, event->xconfigure.height);
      break;
   case KeyPress:
      {
         char buffer[10];
            XLookupString(&event->xkey, buffer, sizeof(buffer),
                          NULL, NULL);
            if (buffer[0] == 27 || buffer[0] == 'q') {
               /* escape */
	       glGetTextureImage(cb, 0, GL_RGBA, GL_UNSIGNED_BYTE, FBOWIDTH * FBOHEIGHT * 4, image);
               return EXIT;
            }
         return DRAW;
      }
   }
   return NOP;
}

double size = 0.8;

static void
event_loop(Display *dpy, Window win)
{
   while (1) {
      int op;
      while (XPending(dpy) > 0) {
         XEvent event;
         XNextEvent(dpy, &event);
         op = handle_event(dpy, win, &event);
         if (op == EXIT)
            return;
         else if (op == DRAW)
            break;
      }

	make_texture(size);
	display_texture();
	glXSwapBuffers(dpy, win);
   }
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

int
main(int argc, char *argv[])
{
   unsigned int winWidth = 300, winHeight = 300;
   int x = 0, y = 0;
   Display *dpy;
   Window win;
   GLXContext ctx;
   char *dpyName = NULL;
   int i;

   if (argc == 2) {
	double s = atof(argv[1]);
	printf("%f\n", s);
	if (0.1 <= s && s < 0.8) { size = s; } }

   dpy = XOpenDisplay(dpyName);
   if (!dpy) {
      printf("Error: couldn't open display %s\n",
	     dpyName ? dpyName : getenv("DISPLAY"));
      return -1; }

   make_window(dpy, "glxgears", x, y, winWidth, winHeight, &win, &ctx);
   XMapWindow(dpy, win);
   glXMakeCurrent(dpy, win, ctx);

   init();

   /* Set initial projection/viewing transformation.
    * We can't be sure we'll get a ConfigureNotify event when the window
    * first appears.
    */
   reshape(winWidth, winHeight);

   event_loop(dpy, win);

   glXMakeCurrent(dpy, None, NULL);
   glXDestroyContext(dpy, ctx);
   XDestroyWindow(dpy, win);
   XCloseDisplay(dpy);

   FILE *fp = fopen("simple.raw", "w");
   fwrite(image, 4, FBOWIDTH * FBOHEIGHT, fp);
   fclose(fp);

   for (int i = 0; i < FBOHEIGHT; i++) {
	for (int j = 0; j < FBOWIDTH; j ++) {
		dot(image[i][j]);
	}
	printf("\n");
   }

   return 0;
}

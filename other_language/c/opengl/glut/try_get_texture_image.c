#include <stdio.h>
#include <stdlib.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/glut.h>

static int width, height;

#define FBOWIDTH 64
#define FBOHEIGHT 32

static GLuint fb;
static GLuint cb;
static GLuint rb;

GLubyte image[FBOHEIGHT][FBOWIDTH][4];

static void init(void)
{

  glGenTextures(1, &cb);
  glBindTexture(GL_TEXTURE_2D, cb);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FBOWIDTH, FBOHEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindTexture(GL_TEXTURE_2D, 0);

  glGenRenderbuffers(1, &rb);
  glBindRenderbuffer(GL_RENDERBUFFER, rb);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, FBOWIDTH, FBOHEIGHT);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  glGenFramebuffers(1, &fb);
  glBindFramebuffer(GL_FRAMEBUFFER, fb);

  glFramebufferTexture2DEXT(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, cb, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, rb);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

  glEnable(GL_LIGHT0);
}

static void display(void)
{
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(30.0, (GLdouble)width / (GLdouble)height, 1.0, 10.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(3.0, 4.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  
  glViewport(0, 0, FBOWIDTH, FBOHEIGHT);

  glEnable(GL_DEPTH_TEST);

  glEnable(GL_LIGHTING);

  glBindFramebuffer(GL_FRAMEBUFFER, fb);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  glutSolidTeapot(1.0);
  glFlush();

  glGetTextureImage(cb, 0, GL_RGBA, GL_UNSIGNED_BYTE, FBOWIDTH * FBOHEIGHT * 4, image);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

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

  glFlush();
}

static void resize(int w, int h)
{
  width = w;
  height = h;
}

void
dot(GLubyte px[])
{
	int n = (int)px[0] + (int)px[1] + (int)px[2];
	if (n < 0xc0) printf(" ");
	else if (n < 0x180) printf(".");
	else if (n < 0x240) printf("+");
	else printf("*");
}

static void keyboard(unsigned char key, int x, int y)
{
  switch (key) {
  case 'q':
  case 'Q':
  case '\033':
	for (int i = 0; i < FBOHEIGHT; i++) {
		for (int j = 0; j < FBOWIDTH; j++) {
			for (int k = 0; k < 4; k++) {
				printf("%02x", image[i][j][k]);
			}
			printf(" ");
		}
		printf("\n");
	}
	for (int i = 0; i < FBOHEIGHT; i++) {
		for (int j = 0; j < FBOWIDTH; j++) {
			dot(image[i][j]);
			printf(" ");
		}
		printf("\n");
	}
    exit(0);
  default:
    break;
  }
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH);
  glutCreateWindow("FBO Sample");
  glutDisplayFunc(display);
  glutReshapeFunc(resize);
  glutKeyboardFunc(keyboard);
  init();
  glutMainLoop();

  return 0;
}

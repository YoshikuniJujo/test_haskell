#include <stdio.h>
#include <stdlib.h>

#if defined(WIN32)
//#  pragma comment(linker, "/subsystem:\"windows\" /entry:\"mainCRTStartup\"")
#  pragma comment(lib, "glew32.lib")
#  include "glew.h"
#  include "glut.h"
#elif defined(__APPLE__) || defined(MACOSX)
#  include <GLUT/glut.h>
#else
#  define GL_GLEXT_PROTOTYPES
#  include <GL/glut.h>
#endif

static int width, height;   // ?E?B???h?E?̕??ƍ???

#define FBOWIDTH 16
#define FBOHEIGHT 32

static GLuint fb;
static GLuint cb;
static GLuint rb;

GLubyte image[32][16][4];

/*
** ??????
*/
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

  // ?w?i?F
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

  // ????
  glEnable(GL_LIGHT0);
}

/*
** ???ʕ\??
*/
static void display(void)
{
  // ?????ϊ??s???̐ݒ?
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(30.0, (GLdouble)width / (GLdouble)height, 1.0, 10.0);

  // ???f???r???[?ϊ??s???̐ݒ?
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(3.0, 4.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  
  // ?r???[?|?[?g?̐ݒ?
  glViewport(0, 0, FBOWIDTH, FBOHEIGHT);

  // ?B?ʏ��????L???ɂ???
  glEnable(GL_DEPTH_TEST);

  // ?A?e?t?????L???ɂ???
  glEnable(GL_LIGHTING);

  glBindFramebuffer(GL_FRAMEBUFFER, fb);

  // ?J???[?o?b?t?@?ƃf?v?X?o?b?t?@???N???A
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  // ?V?[???̕`??
  glutSolidTeapot(1.0);
  glFlush();

  glGetTextureImage(cb, 0, GL_RGBA, GL_UNSIGNED_BYTE, 32 * 32 * 4, image);

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

/*
** ?E?B???h?E?T?C?Y?̕ύX
*/
static void resize(int w, int h)
{
  // ?E?B???h?E?̃T?C?Y???ۑ?????
  width = w;
  height = h;
}

/*
** ?L?[?{?[?h????
*/
static void keyboard(unsigned char key, int x, int y)
{
  switch (key) {
  case 'q':
  case 'Q':
  case '\033':
    // ESC ?? q ?? Q ???^?C?v???????I??
	for (int i = 0; i < 32; i++) {
  		for (int j = 0; j < 16; j++) {
			for (int k = 0; k < 4; k++) {
				printf("%02x", image[i][j][k]);
			}
			printf(" ");
		}
		printf("\n");
	}
    exit(0);
  default:
    break;
  }
}

/*
** ???C???v???O????
*/
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

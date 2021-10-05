#include <stdio.h>
#include <GL/glut.h>

#define MAXPOINTS 100
GLint point[MAXPOINTS][2];
int pointnum = 0;
int rubberband = 0;

void
display (void)
{
	glClear(GL_COLOR_BUFFER_BIT);
	glutWireTeapot(0.5);
	glFlush();
}

void
keyboard(unsigned char key, int x, int y)
{
	switch (key) {
	case 'q': case 'Q': case '\033': exit(0);
	default: printf("%d\n", key); break; }
}

void
init(void)
{
	glClearColor(0.0, 0.0, 0.0, 1.0);
}

int
main(int argc, char *argv[])
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGBA);
	glutCreateWindow(argv[0]);
	glutDisplayFunc(display);
	glutKeyboardFunc(keyboard);
	init();
	glutMainLoop();
	return 0;
}

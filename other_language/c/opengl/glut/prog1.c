#include <stdio.h>
#include <GL/glut.h>

#define MAXPOINTS 100
GLint point[MAXPOINTS][2];
int pointnum = 0;
int rubberband = 0;

void
display (void)
{
	int i;

	glClear(GL_COLOR_BUFFER_BIT);
	glBegin(GL_LINES);
	for (i = 0; i < pointnum; ++i) {
		glVertex2iv(point[i]);
	}
	glEnd();
	glFlush();
}

void
resize(int w, int h)
{
	glViewport(0, 0, w, h);
	glLoadIdentity();
	glOrtho(-0.5, (GLdouble)w - 0.5, (GLdouble)h - 0.5, -0.5, -1.0, 1.0);
}

void
mouse(int button, int state, int x, int y)
{
	static int x0, y0;

	switch (button) {
	case GLUT_LEFT_BUTTON:
		point[pointnum][0] = x;
		point[pointnum][1] = y;
		if (state == GLUT_UP) {
			glColor3d(0.0, 0.0, 0.0);
			glBegin(GL_LINES);
			glVertex2iv(point[pointnum - 1]);
			glVertex2iv(point[pointnum]);
			glEnd();
			glFlush();
			rubberband = 0;
			}
		if (pointnum < MAXPOINTS - 1) ++ pointnum;
		break;
	case GLUT_MIDDLE_BUTTON:
		break;
	case GLUT_RIGHT_BUTTON:
		break;
	default:
		break; }
}

void
motion(int x, int y)
{
	static GLint savepoint[2];

	glEnable(GL_COLOR_LOGIC_OP);
	glLogicOp(GL_INVERT);

	glBegin(GL_LINES);
	if (rubberband) {
		glVertex2iv(point[pointnum - 1]);
		glVertex2iv(savepoint);
	}

	glVertex2iv(point[pointnum - 1]);
	glVertex2i(x, y);

	glEnd();
	glFlush();

	glLogicOp(GL_COPY);
	glDisable(GL_COLOR_LOGIC_OP);

	savepoint[0] = x;
	savepoint[1] = y;

	rubberband = 1;
}

void
keyboard(unsigned char key, int x, int y)
{
	switch (key) {
	case 'q':
	case 'Q':
	case '\033':
		exit(0);
	default:
		printf("%d\n", key);
		break; }
}

void
init(void)
{
	glClearColor(1.0, 1.0, 1.0, 1.0);
}

int
main(int argc, char *argv[])
{
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(320, 240);
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGBA);
	glutCreateWindow(argv[0]);
	glutDisplayFunc(display);
	glutReshapeFunc(resize);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	glutKeyboardFunc(keyboard);
	init();
	glutMainLoop();
	return 0;
}

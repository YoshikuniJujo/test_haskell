#if defined(WIN32)
//#  pragma comment(linker, "/subsystem:\"windows\" /entry:\"mainCRTStartup\"")
#  include "glut.h"
#elif defined(__APPLE__) || defined(MACOSX)
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif

#include "box.h"

/*
** Ȣ������
*/
void box(double x, double y, double z)
{
  /* ĺ���κ�ɸ�� */
  const GLdouble vertex[][4][3] = {
    {{ -x, -y, -z }, {  x, -y, -z }, {  x, -y,  z }, { -x, -y,  z }},
    {{ -x, -y, -z }, { -x,  y, -z }, {  x,  y, -z }, {  x, -y, -z }},
    {{  x, -y, -z }, {  x,  y, -z }, {  x,  y,  z }, {  x, -y,  z }},
    {{  x, -y,  z }, {  x,  y,  z }, { -x,  y,  z }, { -x, -y,  z }},
    {{ -x, -y,  z }, { -x,  y,  z }, { -x,  y, -z }, { -x, -y, -z }},
    {{ -x,  y,  z }, {  x,  y,  z }, {  x,  y, -z }, { -x,  y, -z }},
  };
  
  /* ĺ���Υƥ��������ɸ */
  static const GLdouble texcoord[][4][2] = {
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
  };
  
  /* �̤�ˡ���٥��ȥ� */
  static const GLdouble normal[][3] = {
    {  0.0, -1.0,  0.0 },
    {  0.0,  0.0, -1.0 },
    {  1.0,  0.0,  0.0 },
    {  0.0,  0.0,  1.0 },
    { -1.0,  0.0,  0.0 },
    {  0.0,  1.0,  0.0 },
  };
  
  int i, j;
  
  /* �ͳѷ������Ȣ������ */
  glBegin(GL_QUADS);
  for (j = 0; j < 6; ++j) {
    glNormal3dv(normal[j]);
    for (i = 0; i < 4; ++i) {
      /* �ƥ��������ɸ�λ��� */
      glTexCoord3dv(vertex[j][i]);
      /* �б�����ĺ����ɸ�λ��� */
      glVertex3dv(vertex[j][i]);
    }
  }
  glEnd();
}

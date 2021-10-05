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
** 箱の描画
*/
void box(double x, double y, double z)
{
  /* 頂点の座標値 */
  const GLdouble vertex[][4][3] = {
    {{ -x, -y, -z }, {  x, -y, -z }, {  x, -y,  z }, { -x, -y,  z }},
    {{ -x, -y, -z }, { -x,  y, -z }, {  x,  y, -z }, {  x, -y, -z }},
    {{  x, -y, -z }, {  x,  y, -z }, {  x,  y,  z }, {  x, -y,  z }},
    {{  x, -y,  z }, {  x,  y,  z }, { -x,  y,  z }, { -x, -y,  z }},
    {{ -x, -y,  z }, { -x,  y,  z }, { -x,  y, -z }, { -x, -y, -z }},
    {{ -x,  y,  z }, {  x,  y,  z }, {  x,  y, -z }, { -x,  y, -z }},
  };
  
  /* 頂点のテクスチャ座標 */
  static const GLdouble texcoord[][4][2] = {
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
    {{ 0.0, 0.0 }, { 0.0, 1.0 }, { 1.0, 1.0 }, { 1.0, 0.0 }},
  };
  
  /* 面の法線ベクトル */
  static const GLdouble normal[][3] = {
    {  0.0, -1.0,  0.0 },
    {  0.0,  0.0, -1.0 },
    {  1.0,  0.0,  0.0 },
    {  0.0,  0.0,  1.0 },
    { -1.0,  0.0,  0.0 },
    {  0.0,  1.0,  0.0 },
  };
  
  int i, j;
  
  /* 四角形６枚で箱を描く */
  glBegin(GL_QUADS);
  for (j = 0; j < 6; ++j) {
    glNormal3dv(normal[j]);
    for (i = 0; i < 4; ++i) {
      /* テクスチャ座標の指定 */
      glTexCoord3dv(vertex[j][i]);
      /* 対応する頂点座標の指定 */
      glVertex3dv(vertex[j][i]);
    }
  }
  glEnd();
}

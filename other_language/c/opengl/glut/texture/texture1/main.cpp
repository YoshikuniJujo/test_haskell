#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#if defined(WIN32)
//#  pragma comment(linker, "/subsystem:\"windows\" /entry:\"mainCRTStartup\"")
#  include "glut.h"
#elif defined(__APPLE__) || defined(MACOSX)
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif

/*
** 光源
*/
static const GLfloat lightpos[] = { 0.0, 0.0, 1.0, 0.0 }; /* 位置　　　 */
static const GLfloat lightcol[] = { 1.0, 1.0, 1.0, 1.0 }; /* 直接光強度 */
static const GLfloat lightamb[] = { 0.1, 0.1, 0.1, 1.0 }; /* 環境光強度 */

/*
** テクスチャ
*/
#define TEXWIDTH  256                      /* テクスチャの幅　　　 */
#define TEXHEIGHT 256                      /* テクスチャの高さ　　 */
static const char texture1[] = "tire.raw"; /* テクスチャファイル名 */

/*
** 初期化
*/
static void init(void)
{
  /* テクスチャの読み込みに使う配列 */
  GLubyte texture[TEXHEIGHT][TEXWIDTH][4];
  FILE *fp;
  
  /* テクスチャ画像の読み込み */
  if ((fp = fopen(texture1, "rb")) != NULL) {
    fread(texture, sizeof texture, 1, fp);
    fclose(fp);
  }
  else {
    perror(texture1);
  }
  
  /* テクスチャ画像はバイト単位に詰め込まれている */
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  
  /* テクスチャの割り当て */
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TEXWIDTH, TEXHEIGHT, 0,
               GL_RGBA, GL_UNSIGNED_BYTE, texture);
  
  /* テクスチャを拡大・縮小する方法の指定 */
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  
  /* テクスチャの繰り返し方法の指定 */
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  
  /* テクスチャ環境 */
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  
#if 0
  /* 混合する色の設定 */
  static const GLfloat blend[] = { 0.0, 1.0, 0.0, 1.0 };
  glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, blend);
#endif
  
  /* アルファテストの判別関数 */
  glAlphaFunc(GL_GREATER, 0.5);
  
  /* 初期設定 */
  glClearColor(0.3, 0.3, 1.0, 0.0);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  
  /* 光源の初期設定 */
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightcol);
  glLightfv(GL_LIGHT0, GL_SPECULAR, lightcol);
  glLightfv(GL_LIGHT0, GL_AMBIENT, lightamb);
}

/* 箱を描く関数の宣言 */
#include "box.h"

/*
** シーンの描画
*/
static void scene(void)
{
  static const GLfloat color[] = { 1.0, 1.0, 1.0, 1.0 };  /* 材質 (色) */
  
  /* 材質の設定 */
  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, color);
  
  /* アルファテスト開始 */
  glEnable(GL_ALPHA_TEST);
  
  /* テクスチャマッピング開始 */
  glEnable(GL_TEXTURE_2D);
  
  /* 箱を描く */
  box(1.0, 1.0, 1.0);
  
  /* テクスチャマッピング終了 */
  glDisable(GL_TEXTURE_2D);
  
  /* アルファテスト終了 */
  glDisable(GL_ALPHA_TEST);
}


/****************************
** GLUT のコールバック関数 **
****************************/

/* トラックボール処理用関数の宣言 */
#include "trackball.h"

/* アニメーションのサイクル */
#define FRAMES 360

static void display(void)
{
  /* フレーム数をカウントして時間として使う */
  static int frame = 0;                      /* フレーム数　　　　　　　 */
  double t = (double)frame / (double)FRAMES; /* 時間とともに 0→1 に変化 */
  
  if (++frame >= FRAMES) frame = 0;
  
  /* テクスチャ行列の設定 */
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslated(0.5, 0.5, 0.0);
  glRotated(t * 360.0, 0.0, 0.0, 1.0);
  glScaled(0.5, 0.5, 1.0);
  gluPerspective(60.0, 1.0, 1.0, 100.0);
  gluLookAt(0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0);
  
  /* モデルビュー変換行列の設定 */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  
  /* 光源の位置を設定 */
  glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
  
  /* 視点の移動（物体の方を奥に移動）*/
  glTranslated(0.0, 0.0, -3.0);
  
  /* トラックボール処理による回転 */
  glMultMatrixd(trackballRotation());
  
  /* 画面クリア */
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  /* シーンの描画 */
  scene();
  
  /* ダブルバッファリング */
  glutSwapBuffers();
}

static void resize(int w, int h)
{
  /* トラックボールする範囲 */
  trackballRegion(w, h);
  
  /* ウィンドウ全体をビューポートにする */
  glViewport(0, 0, w, h);
  
  /* 透視変換行列の指定 */
  glMatrixMode(GL_PROJECTION);
  
  /* 透視変換行列の初期化 */
  glLoadIdentity();
  gluPerspective(60.0, (double)w / (double)h, 1.0, 100.0);
}

static void idle(void)
{
  /* 画面の描き替え */
  glutPostRedisplay();
}

static void mouse(int button, int state, int x, int y)
{
  switch (button) {
  case GLUT_LEFT_BUTTON:
    switch (state) {
    case GLUT_DOWN:
      /* トラックボール開始 */
      trackballStart(x, y);
      break;
    case GLUT_UP:
      /* トラックボール停止 */
      trackballStop(x, y);
      break;
    default:
      break;
    }
    break;
    default:
      break;
  }
}

static void motion(int x, int y)
{
  /* トラックボール移動 */
  trackballMotion(x, y);
}

static void keyboard(unsigned char key, int x, int y)
{
  switch (key) {
  case 'q':
  case 'Q':
  case '\033':
    /* ESC か q か Q をタイプしたら終了 */
    exit(0);
  default:
    break;
  }
}

/*
** メインプログラム
*/
int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);
  glutCreateWindow(argv[0]);
  glutDisplayFunc(display);
  glutReshapeFunc(resize);
  glutIdleFunc(idle);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);
  glutKeyboardFunc(keyboard);
  init();
  glutMainLoop();
  return 0;
}

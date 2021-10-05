/*
** 簡易トラックボール処理
*/
extern void trackballInit(void);
extern void trackballRegion(int w, int h);
extern void trackballStart(int x, int y);
extern void trackballMotion(int x, int y);
extern void trackballStop(int x, int y);
extern double *trackballRotation(void);

int hm_left(int x, int y) { return x; }
int hm_right(int x, int y) { return x + 2; }
int hm_top(int x, int y) { return y; }
int hm_bottom(int x, int y) { return y + 2; }

int hm_x_from_left(int l) { return l; }
int hm_x_from_right(int r) { return r - 2; }
int hm_y_from_top(int t) { return t; }
int hm_y_from_bottom (int b) { return b - 2; }

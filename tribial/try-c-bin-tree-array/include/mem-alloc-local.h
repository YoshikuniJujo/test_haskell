#ifndef _MEM_ALLOC_LOCAL_H
#define _MEM_ALLOC_LOCAL_H

int get_index(int sz, int dp, int i, int ret);
int normalize(int sz);
void put_used(int i, int n, char *buf);

#endif

#ifndef _LINKED_LIST_H
#define _LINKED_LIST_H

typedef struct Node { void *value; struct Node *next; } Node;
typedef Node *LinkedList;

LinkedList *ll_list_new(void);

#endif

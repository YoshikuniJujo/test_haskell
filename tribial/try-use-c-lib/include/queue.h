#ifndef _QUEUE_H
#define _QUEUE_H

#include <stddef.h>
#include <stdbool.h>

typedef struct List { struct List *prev; void *value; } List;
typedef struct { List *head; List *last; } Queue;

Queue *queue_new(void);
void queue_destroy(Queue *queue);

bool null(Queue *queue);
void cons(void *value, Queue *queue);
void *unsnoc(Queue *queue);

#define EmptyList NULL

#endif

#include <stdlib.h>
#include <stdbool.h>
#include <queue.h>

Queue*
queue_new(void)
{
	Queue *q = (Queue *)malloc(sizeof(Queue));
	q->head = EmptyList;
	q->last = EmptyList;

	return q;
}

void
queue_destroy(Queue* queue)
{
	free(queue);
}

bool
null(Queue *queue)
{
	if (queue->head == EmptyList || queue->last == EmptyList) {
		return true;
	} else {
		return false;
	}
}

void
cons(void *value, Queue *queue)
{
	List *hd = queue->head;
	List *nl = (List *)malloc(sizeof(List));

	nl->value = value;

	if (hd == EmptyList && queue->last == EmptyList) {
		queue->head = nl;
		queue->last = nl;
	} else {
		hd->prev = nl;
		queue->head = nl;
	}
}

void*
unsnoc(Queue *queue)
{
	List *lst = queue->last;
	void *v;

	if (lst == NULL) return NULL;
	v = lst->value;

	queue->last = lst->prev;
	if (lst->prev == EmptyList) { queue->head = EmptyList; }
	free(lst);

	return v;
}

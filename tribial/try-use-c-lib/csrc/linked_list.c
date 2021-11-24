#include <stdlib.h>
#include <linked_list.h>

LinkedList*
ll_list_new(void)
{
	LinkedList *lst;
	lst = (LinkedList *)malloc(sizeof(LinkedList));
	*lst = NULL;
	return lst;
}

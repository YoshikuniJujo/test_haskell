#include <stdio.h>
#include <string.h>
#include "trump.h"

void print_card(card *c) {
	char suit_name[10], rank_name[10];
	switch (c->suit) {
	case Spade:
		strcpy(suit_name, "Spade");
		break;
	case Heart:
		strcpy(suit_name, "Heart");
		break;
	case Diamond:
		strcpy(suit_name, "Diamond");
		break;
	case Club:
		strcpy(suit_name, "Club");
		break;
	}
	switch (c->rank) {
	case 1:
		strcpy(rank_name, "Ace");
		break;
	case 11:
		strcpy(rank_name, "Jack");
		break;
	case 12:
		strcpy(rank_name, "Queen");
		break;
	case 13:
		strcpy(rank_name, "King");
		break;
	default:
		sprintf(rank_name, "%d", c->rank);
		break;
	}
	printf("%s %s\n", suit_name, rank_name);
}

typedef enum SUIT { Spade, Heart, Diamond, Club } SUIT;

typedef struct card {
	SUIT suit;
	int rank;
} card;

void print_card(card *);

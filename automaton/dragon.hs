import Automaton

data Room = Dragon | A | B | Enter deriving Show

next :: Room -> Door -> Room
next Enter White = A
next Enter Black = B
next A White = Enter
next A Black = Dragon
next B White = Dragon
next B Black = Enter
next Dragon Black = A
next Dragon White = B

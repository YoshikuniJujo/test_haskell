import Automaton

data Room = Enter | A | Exit deriving Show

next :: Room -> Door -> Room
next Enter Black = Enter
next Enter White = A
next A Black = Exit
next A White = A
next Exit Black = Enter
next Exit White = A

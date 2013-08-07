import Automaton

data Room = A | B | C | D | E | F | G | H | I | J | K | L | M | N deriving Show

next :: Room -> Door -> [Room]
next A White = [B, M]
next B White = [C, L]
next C White = [D, K]
next D White = [E, J]
next E White = [F, I]
next F White = [G, H]
next H Black = [I]
next I Black = [J]
next J Black = [K]
next K Black = [L]
next L Black = [M]
next M Black = [N]
next _ _ = []

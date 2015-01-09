data Door = Gold | Silver | Copper deriving Show

data Room = Enter | A | B | C | D | E | F | G | H | I | J | K | Exit deriving Show

next :: Room -> Door -> Room
next Enter Gold = J
next Enter Silver = E
next Enter Copper = A
next A Gold = K
next A Silver = F
next A Copper = B
next B Silver = G
next B Copper = C
next C Silver = H
next C Copper = D
next D Silver = I
next D Copper = E
next E Silver = J
next E Copper = F
next F Silver = K
next F Copper = G
next G Copper = H
next H Copper = I
next J Copper = K
next _ _ = Exit

ctd :: Char -> Door
ctd 'g' = Gold
ctd 's' = Silver
ctd 'c' = Copper

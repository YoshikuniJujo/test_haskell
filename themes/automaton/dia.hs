data Item = Black | White | Ticket deriving Show

next :: [Item] -> Item -> [[Item]]
next [] dia = next [Ticket] dia
next (Ticket : items) dia =
	next (White : Ticket : Black : items) dia ++
 	next (White : Black : items) dia
next (White : items) White = [items]
next (Black : items) Black = [items]
next _ _ = []

ctd :: Char -> Item
ctd '*' = Black
ctd '.' = White

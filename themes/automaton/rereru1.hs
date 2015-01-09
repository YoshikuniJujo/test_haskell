data Item = White | Black | Ticket deriving Show

next :: [Item] -> Item -> [[Item]]
-- next [] stone = next [Ticket] stone
next (Ticket : items) stone =
	next (White : White : items) stone ++
	next (Black : Black : items) stone ++
	next (White : Ticket : White : items) stone ++
	next (Black : Ticket : Black : items) stone
next (White : items) White = [items]
next (Black : items) Black = [items]
next _ _ = []

ctd :: Char -> Item
ctd '*' = Black
ctd '.' = White

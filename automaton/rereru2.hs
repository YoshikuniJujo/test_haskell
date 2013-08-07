data Stone = White | Black deriving Show

data Room = Enter | B | C | Exit deriving Show

next :: (Room, [Stone]) -> Stone -> [(Room, [Stone])]
next (Enter, []) stone = next (B, []) stone
next (B, White : stones) White = [
	(C, stones),
	(B, White : White : stones)]
next (B, Black : stones) Black = [
	(C, stones),
	(B, Black : Black : stones)]
next (B, stones) stone = [(B, stone : stones)]
next (C, [White]) White = [(Exit, [])]
next (C, [Black]) Black = [(Exit, [])]
next (C, White : stones) White = [(C, stones)]
next (C, Black : stones) Black = [(C, stones)]
next _ _ = []

ctd :: Char -> Stone
ctd '*' = Black
ctd '.' = White

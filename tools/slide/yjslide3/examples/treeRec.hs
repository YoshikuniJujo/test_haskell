type Paths = [(Char, [Char])]

paths :: Paths
paths = [('a', ['b', 'c']), ('b', ['d', 'e']), ('e', ['f'])]

existPath :: Paths -> Char -> Char -> Bool
existPath ps nb ne
	| nb == ne = True
	| otherwise = case lookup nb ps of
		Just ns -> any (flip (existPath ps) ne) ns
		_ -> False

type Paths2 = [(Char, (Char, Char))]

paths2 :: Paths2
paths2 = [('a', ('b', 'c')), ('b', ('d', 'e')), ('e', ('f', 'g'))]

existPath2 :: Paths2 -> Char -> Char -> Bool
existPath2 ps nb ne
	| nb == ne = True
	| otherwise = case lookup nb ps of
		Just (nl, nr) -> existPath2 ps nl ne || existPath2 ps nr ne
		_ -> False

distance :: Paths2 -> Char -> Char -> Maybe Int
distance ps nb ne
	| nb == ne = Just 0
	| otherwise = case lookup nb ps of
		Just (nl, nr) -> fmap (+ 1) $ selectJust (distance ps nl ne) (distance ps nr ne)
		_ -> Nothing

selectJust :: Maybe a -> Maybe a -> Maybe a
selectJust (Just x) _ = Just x
selectJust _ (Just y) = Just y
selectJust _ _ = Nothing

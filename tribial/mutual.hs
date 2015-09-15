{-# LANGUAGE TupleSections #-}

import Data.Char

type Parse a = String -> [(a, String)]

list, list1 :: Parse a -> Parse [a]
list p = succeed [] /// list1 p
list1 p = (p --> list p) `build` (uncurry (:))

succeed :: a -> Parse a
succeed v s = [(v, s)]

(///) :: Parse a -> Parse a -> Parse a
(p1 /// p2) s = p1 s ++ p2 s

(-->) :: Parse a -> Parse b -> Parse (a, b)
-- (p1 --> p2) s = [ ((x, y), r') | (x, r) <- p1 s, (y, r') <- p2 r ]
(p1 --> p2) s = (\(x, r) -> (\(y, r') -> ((x, y), r')) `map` p2 r) `concatMap` p1 s

build :: Parse a -> (a -> b) -> Parse b
build p f s = (\(v, r) -> (f v, r)) `map` p s

number :: Parse Int
number s@(c : cs)
	| isSpace c = number cs
	| isDigit c = [(read ds, rs)]
	where
	(ds, rs) = span isDigit s
number _ = []

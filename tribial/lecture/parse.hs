import Data.Maybe
import Data.Char

type Parse a = String -> [(a, String)]

succeed :: a -> Parse a
succeed v i = [(v, i)]

check :: (Char -> Bool) -> Parse Char
check p (c : cs) | p c = [(c, cs)]
check _ _ = []

char :: Char -> Parse Char
char = check . (==)

alt :: Parse a -> Parse a -> Parse a
(p1 `alt` p2) i = p1 i ++ p2 i

build :: Parse a -> (a -> b) -> Parse b
build p f i = [ (f x, r) | (x, r) <- p i ]

(>@>) :: Parse a -> Parse b -> Parse (a, b)
(p1 >@> p2) i = [ ((x, y), r') | (x, r) <- p1 i, (y, r') <- p2 r ]

(>@) :: Parse a -> Parse b -> Parse a
p1 >@ p2 = (p1 >@> p2) `build` fst

(@>) :: Parse a -> Parse b -> Parse b
p1 @> p2 = (p1 >@> p2) `build` snd

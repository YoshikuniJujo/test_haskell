module Parse (
	Parse, succeed, spot, char, alt, build, (>*>), (>*), (*>), eof,
	list, list1 ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow
import Control.Monad

infixr 3 `alt`
infixl 5 `build`
infixr 7 >*>, >*, *>

type Parse a = String -> [(a, String)]

succeed :: a -> Parse a
succeed v i = return (v, i)

spot :: (Char -> Bool) -> Parse Char
spot p (c : cs) | p c = return (c, cs)
spot _ _ = fail ""

char :: Char -> Parse Char
char = spot . (==)

alt :: Parse a -> Parse a -> Parse a
p1 `alt` p2 = mplus <$> p1 <*> p2

build :: Parse a -> (a -> b) -> Parse b
build p f = ((f `first`) <$>) . p

(>*>) :: Parse a -> Parse b -> Parse (a, b)
(p1 >*> p2) i = do
	(x, r) <- p1 i
	(y, r') <- p2 r
	return ((x, y), r')

(>*) :: Parse a -> Parse b -> Parse a
p1 >* p2 = p1 >*> p2 `build` fst

(*>) :: Parse a -> Parse b -> Parse b
p1 *> p2 = p1 >*> p2 `build` snd

eof :: Parse ()
eof "" = return ((), "")
eof _ = fail ""

list, list1 :: Parse a -> Parse [a]
list p = succeed [] `alt` list1 p
list1 p = p >*> list p `build` uncurry (:)



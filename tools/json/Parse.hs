module Parse (
	Parse, succeed, spot, char, alt, build, (>*>), (>*), (*>), eof,
	list, list1 ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow
import Control.Monad
import Data.Bool

infixr 3 `alt`
infixl 5 `build`
infixr 7 >*>, >*, *>

type Parse t a = [t] -> [(a, [t])]

succeed :: a -> Parse t a
succeed = (return .) . (,)

spot :: (t -> Bool) -> Parse t t
spot p (c : cs) | p c = return (c, cs)
spot _ _ = fail ""

char :: Eq t => t -> Parse t t
char = spot . (==)

alt :: Parse t a -> Parse t a -> Parse t a
alt = (<*>) . (mplus <$>)

build :: Parse t a -> (a -> b) -> Parse t b
build = (. (<$>) . first) . flip (.)

(>*>) :: Parse t a -> Parse t b -> Parse t (a, b)
(p1 >*> p2) i = [ ((x, y), r') | (x, r) <- p1 i, (y, r') <- p2 r ]

(>*) :: Parse t a -> Parse t b -> Parse t a
(>*) = ((`build` fst) .) . (>*>)

(*>) :: Parse t a -> Parse t b -> Parse t b
(*>) = ((`build` snd) .) . (>*>)

eof :: Parse t ()
eof = bool (return ((), [])) (fail "") . null

list, list1 :: Parse t a -> Parse t [a]
list = (succeed [] `alt`) . list1
list1 = (`build` uncurry (:)) . ((>*>) <$> id <*> list)

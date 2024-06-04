module Parse (
        Parse, none, succeed, spot, token, tokens, eof, foo, (>*>), alt, build,
        list1, recL1 ) where

import Prelude

import Data.Functor
import Data.Eq
import Data.Function
import Data.Tuple
import Data.List
import Data.Boolean

type Parse a b = List a -> List (Tuple b (List a))

none :: forall a b . Parse a b
none _ = Nil

succeed :: forall a b . b -> Parse a b
succeed = curry (_ : Nil)

spot :: forall a . (a -> Boolean) -> Parse a a
spot p (x : xs) | p x = Tuple x xs : Nil
spot _ _ = Nil

token :: forall a . Eq a => a -> Parse a a
token = spot <<< (\x -> (x == _))

tokens :: forall a . Eq a => List a -> Parse a (List a)
tokens Nil = succeed Nil
tokens (x : xs) = (token x >*> tokens xs) `build` uncurry (:)

eof :: forall a . Parse a Unit
eof Nil = Tuple unit Nil : Nil
eof _ = Nil

foo :: forall a b c . Parse a b -> Parse a c -> Parse a (Tuple b c)
foo p q inp = do
        Tuple x r <- p inp
        Tuple y r' <- q r
        pure $ Tuple (Tuple x y) r'

infixr 8 foo as >*>

alt :: forall a b . Parse a b -> Parse a b -> Parse a b
alt p1 p2 = uncurry (<>) <<< (p1 &&& p2)

build :: forall a b c . Parse a b -> (b -> c) -> Parse a c
build p f = map (first f) <<< p

-- infix 7 build as build

first :: forall a b c . (a -> b) -> Tuple a c -> Tuple b c
first f (Tuple x y) = Tuple (f x) y

list1 :: forall a b . Parse a b -> Parse a (List b)
list1 p = (p `build` (_ : Nil)) `alt` ((p >*> list1 p) `build` uncurry (\x y -> x : y))

bar :: forall a b c . (a -> b) -> (a -> c) -> a -> Tuple b c
bar f g x = Tuple (f x) (g x)

infix 9 bar as &&&

recL1 :: forall a b . (b -> b -> b) -> Parse a b -> Parse a b
recL1 f p = (p >*> recL1' f p) `build` (\(Tuple x xs) -> xs x)

recL1' :: forall a b c . (b -> c -> b) -> Parse a c -> Parse a (b -> b)
recL1' f p = succeed id `alt`
        ((p >*> recL1' f p) `build` (\(Tuple x xs) y -> xs (f y x)))

id :: forall a . a -> a
id x = x

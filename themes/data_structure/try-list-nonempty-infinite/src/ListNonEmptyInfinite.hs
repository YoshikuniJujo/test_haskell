{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ListNonEmptyInfinite where

import Prelude hiding (
	(++), head, tail, init, last, null, length, reverse,
	iterate, repeat, cycle, take, drop )
import Control.Arrow

data List a = Nil | NE (NonEmpty a) deriving Show

data NonEmpty a = a :. List a deriving Show

data Infinite a = a :~ Infinite a

{-
instance Show a => Show (Infinite a) where
	show xs = "[" ++ take 32 xs
	-}

infixr 5 `cons`, .:
class Cons l where cons :: a -> l a -> l a

instance Cons List where
	cons x = \case
		Nil -> NE $ x :. Nil
		NE xs -> NE $ x `cons` xs

instance Cons NonEmpty where x `cons` xs = x :. NE xs
instance Cons Infinite where cons = (:~)

(.:) :: Cons l => a -> l a -> l a
(.:) = cons

singleton :: a -> NonEmpty a
singleton = (:. Nil)

class Add l r where
	type Added l r a
	(++) :: Cons r => l a -> r a -> Added l r a

instance Cons r => Add List r where
	type Added List r a = r a
	Nil ++ ys = ys
	NE xs ++ ys = xs ++ ys

instance Cons r => Add NonEmpty r where
	type Added NonEmpty r a = r a
	(x :. xs) ++ ys = x .: (xs ++ ys)

class Uncons l where
	type Unconsed l a
	uncons :: l a -> (a, Unconsed l a)

instance Uncons NonEmpty where
	type Unconsed NonEmpty a = List a
	uncons (x :. xs) = (x, xs)

instance Uncons Infinite where
	type Unconsed Infinite a = Infinite a
	uncons (x :~ xs) = (x, xs)

head :: Uncons l => l a -> a
head = fst . uncons

tail :: Uncons l => l a -> Unconsed l a
tail = snd . uncons

unsnoc :: NonEmpty a -> (List a, a)
unsnoc (x :. Nil) = (Nil, x)
unsnoc (x :. NE xs) = (NE . (x :.)) `first` unsnoc xs

last :: NonEmpty a -> a
last = snd . unsnoc

init :: NonEmpty a -> List a
init = fst . unsnoc

null :: List a -> Bool
null Nil = True
null (NE _) = False

class Length l where length :: l a -> Int

instance Length List where length Nil = 0; length (NE xs) = length xs
instance Length NonEmpty where length (_ :. xs) = 1 + length xs

instance Functor List where
	_ `fmap` Nil = Nil
	f `fmap` NE xs = NE $ f <$> xs

instance Functor NonEmpty where
	f `fmap` (x :. xs) = f x :. (f <$> xs)

instance Functor Infinite where
	f `fmap` (x :~ xs) = f x :~ (f <$> xs)

class Reverse l where reverse :: l a -> l a

instance Reverse List where
	reverse Nil = Nil
	reverse (NE xs) = NE $ reverse xs

instance Reverse NonEmpty where
	reverse = rev Nil
		where
		rev s (x :. xs) = case xs of
			Nil -> x :. s
			NE xs' -> rev (NE $ x :. s) xs'

iterate :: (a -> a) -> a -> Infinite a
iterate f x = x :~ iterate f (f x)

repeat :: a -> Infinite a
repeat x = x :~ repeat x

cycle :: NonEmpty a -> Infinite a
cycle xs = ccl xs
	where
	ccl (y :. Nil) = y :~ cycle xs
	ccl (y :. NE ys) = y :~ ccl ys

take :: Int -> Infinite a -> List a
take n _ | n < 1 = Nil
take n (x :~ xs) = x .: take (n - 1) xs

drop :: Int -> Infinite a -> Infinite a
drop n xs | n < 1 = xs
drop n (_ :~ xs) = drop n xs

sampleList :: List Int
sampleList = 3 .: 4 .: 5 .: Nil

sampleNonEmpty :: NonEmpty Int
sampleNonEmpty = 6 .: 7 .: 8 .: singleton 9

sampleInfinite1 :: Infinite Int
sampleInfinite1 = 123 :~ sampleInfinite1

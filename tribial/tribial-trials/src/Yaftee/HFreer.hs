{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.HFreer where

import Control.Monad

data H h i o a = Pure a | forall x . h (H h) i o x :>>= (x -> H h i o a)

data Except e f a where
	Throw :: e -> Except e f a
	Catch :: f () () a -> (e -> f () () a) -> Except e f a

data Pipe i o f a where
	Await :: Pipe i o f i
	Yield :: o -> Pipe i o f ()
	(:=$=) :: f i x r -> f x o r' -> Pipe i o f (f i x r, f x o r')
	(:=@=) :: f i x r -> f x o r' -> Pipe i o f (f i x r, f x o r')

instance Functor (H h i o) where
	fmap f = \case
		Pure x -> Pure $ f x
		hx :>>= k -> hx :>>= ((f <$>) . k)

instance Applicative (H h i o) where
	pure = Pure
	Pure f <*> m = f <$> m
	hx :>>= k <*> m = hx :>>= ((<$> m) <=< k)

instance Monad (H h i o) where
	Pure x >>= f = f x
	hx :>>= k >>= f = hx :>>= (f <=< k)

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iteratee where

data Iteratee a b = Done b | Get (a -> Iteratee a b)

instance Functor (Iteratee a) where
	f `fmap` Done x = Done $ f x
	f `fmap` Get k = Get $ (f <$>) . k

instance Applicative (Iteratee a) where
	pure = Done
	Done f <*> mx = f <$> mx
	Get k <*> mx = Get $ ((<$> mx) =<<) . k

instance Monad (Iteratee a) where
	Done x >>= f = f x
	Get k >>= f = Get $ (f =<<) . k

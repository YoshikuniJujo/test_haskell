{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iteratee where

import Control.Concurrent
import System.IO.Unsafe

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

get :: Iteratee a a
get = Get Done

apply :: Iteratee a b -> [a] -> Maybe b
apply (Done b) _ = Just b
apply (Get _) [] = Nothing
apply (Get f) (x : xs) = apply (f x) xs

sample1 :: Iteratee Integer Integer
sample1 = do
	x <- get
	y <- get
	z <- get
	pure $ unsafePerformIO $ threadDelay 1000000 >> pure (x + y + z)

par :: Iteratee a b -> Iteratee a c -> Iteratee a (Iteratee a b, Iteratee a c)
par (Get f) (Get g) = get >>= par <$> f <*> g
par l r = pure (l, r)

value1, value2 :: Integer
Just (Done value1, Done value2) = par sample1 sample1 `apply` [3, 4, 5]

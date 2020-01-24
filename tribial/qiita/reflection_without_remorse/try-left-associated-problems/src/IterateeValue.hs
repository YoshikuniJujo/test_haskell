{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeValue where

import Control.Arrow
import Control.Monad

data Itv i o a = Get o (i -> Itv i o a) | Done a

instance Functor (Itv i o) where
	f `fmap` Done x = Done $ f x
	f `fmap` Get o k = Get o $ (f <$>) . k

instance Applicative (Itv i o) where
	pure = Done
	Done k <*> mx = k <$> mx
	Get o k <*> mx = Get o $ k >=> (<$> mx)

instance Monad (Itv i o) where
	Done x >>= g = g x
	Get o f >>= g = Get o $ f >=> g

getv :: o -> Itv i o i
getv o = Get o pure

sample1, sample2 :: Itv Int Int Int
sample1 = do
	x <- getv 0
	y <- getv x
	pure $ x + y

sample2 = do
	x <- getv 0
	y <- getv x
	z <- getv y
	pure $ x * y * z

addGet :: Int -> Itv Int Int Int
addGet x = getv x >>= \i -> pure (i + x)

addN, addNR :: Int -> Itv Int Int Int
addN n = foldl (>=>) pure (replicate n addGet) 0
addNR n = foldr (>=>) pure (replicate n addGet) 0

feedAll :: Itv i o a -> [i] -> ([o], Maybe a)
feedAll (Done x) _ = ([], Just x)
feedAll _ [] = ([], Nothing)
feedAll (Get o f) (h : t) = (o :) `first` feedAll (f h) t

feedPartial :: Int -> Itv i o a -> Itv i o (Itv i o a)
feedPartial n m | n < 1 = pure m
feedPartial _ m@(Done _) = pure m
feedPartial n (Get o f) = getv o >>= \x -> feedPartial (n - 1) (f x)

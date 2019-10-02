{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeRaw where

import Control.Monad

import ContinuationPassingStyle

data It i a = Get (i -> It i a) | Done a

instance Functor (It i) where
	f `fmap` mx = pure f <*> mx

instance Applicative (It i) where
	pure = Done
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (It i) where
	return = Done
	(Done x) >>= g = g x
	(Get f) >>= g = Get (f >=> g)

get :: It i i
get = Get return

feedAll :: It a b -> [a] -> Maybe b
feedAll (Done a) _ = Just a
feedAll _ [] = Nothing
feedAll (Get f) (h : t) = feedAll (f h) t

addNbad :: Int -> It Int Int
addNbad n = foldl (>>=) get (replicate (n - 1) addGet)
	where addGet x = liftM (+ x) get

testquadratic n = feedAll (addNbad n) [1 .. n]

sumInput :: Int -> It Int Int
sumInput n = Get (foldl (>=>) return (replicate (n - 1) f))
	where f x = get >>= return . (+ x)

testSumInput n = feedAll (sumInput n) [1 .. n]

type ItCo i a = CodensityT (It i) a

getCo :: ItCo i i
getCo = repM get

sumInputCo :: Int -> It Int Int
sumInputCo n = Get $ absM . foldl (>=>) return (replicate (n - 1) f)
	where f x = getCo >>= return . (+ x)

sumInputCo' :: Int -> ItCo Int Int
sumInputCo' n = repM $ Get $ absM . foldl (>=>) return (replicate (n - 1) f)
	where f x = getCo >>= return . (+ x)

testSumInputCo :: Int -> Maybe Int
testSumInputCo n = feedAll (sumInputCo n) [1 .. n]

par :: It i a -> It i b -> It i (It i a, It i b)
par l r
	| Done _ <- l = Done (l, r)
	| Done _ <- r = Done (l, r)
	| Get f <- l, Get g <- r = get >>= \x -> par (f x) (g x)

par10 :: Int -> It Int Int
par10 n = par (sumInput n) (sumInput n) >>= snd

sumPar10 :: Int -> Int -> It Int Int
sumPar10 m n = Get (foldl (>=>) return (replicate (n - 1) f))
	where f x = par10 m >>= return . (+ x)

testSumPar10 :: Int -> Int -> Maybe Int
testSumPar10 m n = sumPar10 m n `feedAll` [1 .. m * n]

parCo :: ItCo i a -> ItCo i b -> ItCo i (ItCo i a, ItCo i b)
parCo l r = repM (par (absM l) (absM r)) >>= \(l', r') -> return (repM l', repM r')

parCo10 :: Int -> ItCo Int Int
parCo10 n = parCo (sumInputCo' n) (sumInputCo' n) >>= snd

sumParCo10 :: Int -> Int -> It Int Int
sumParCo10 m n = Get $ absM . foldl (>=>) return (replicate (n - 1) f)
	where f x = parCo10 m >>= return . (+ x)

testSumParCo10 :: Int -> Int -> Maybe Int
testSumParCo10 m n = sumParCo10 m n `feedAll` [1 .. m * n]

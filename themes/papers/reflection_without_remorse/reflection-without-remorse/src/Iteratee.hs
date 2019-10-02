{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iteratee where

import Control.Monad

import TSequence
import PMonad

data It i a = Get (MCExp (It i) i a) | Done a

instance PMonad (It i) where
	return' = Done
	Done x >>=. g = val g x
	Get f >>=. g = Get $ f |><| g

get :: It i i
get = Get tempty

feedAll :: It a b -> [a] -> Maybe b
feedAll (Done a) _ = Just a
feedAll _ [] = Nothing
feedAll (Get f) (h : t) = feedAll (val f h) t

addNBad :: Int -> It Int Int
addNBad n = foldl (>>=) get (replicate (n - 1) addGet)
	where addGet x = liftM (+ x) get

testquadratic n = feedAll (addNBad n) [1 .. n]

sumInput :: Int -> It Int Int
sumInput n = Get . expr $ foldl (>=>) return (replicate (n - 1) f)
	where f x = get >>= return . (+ x)

testSumInput :: Int -> Maybe Int
testSumInput n = feedAll (sumInput n) [1 .. n]

par :: It i a -> It i b -> It i (It i a, It i b)
par l r
	| Done _ <- l = Done (l, r)
	| Done _ <- r = Done (l, r)
	| Get f <- l, Get g <- r = get >>= \x -> par (val f x) (val g x)

par10 :: Int -> It Int Int
par10 n = par (sumInput n) (sumInput n) >>= snd

sumPar10 :: Int -> Int -> It Int Int
sumPar10 m n = Get . expr $ (foldl (>=>) return (replicate (n - 1) f))
	where f x = par10 m >>= return . (+ x)

testSumPar10 :: Int -> Int -> Maybe Int
testSumPar10 m n = sumPar10 m n `feedAll` [1 .. m * n]

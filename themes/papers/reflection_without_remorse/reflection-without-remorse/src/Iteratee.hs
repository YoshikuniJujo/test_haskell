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

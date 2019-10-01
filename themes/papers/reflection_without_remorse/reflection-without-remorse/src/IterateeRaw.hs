{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeRaw where

import Control.Monad

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

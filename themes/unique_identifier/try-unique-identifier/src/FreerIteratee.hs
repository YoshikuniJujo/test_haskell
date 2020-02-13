{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreerIteratee where

import Freer
import FTCQueue

data It i a where Get :: It i i

type Iteratee i = Freer (It i)

get :: Iteratee i i
get = Get `Bind` tsingleton (MCont pure)

sample :: Iteratee Int Int
sample = do
	x <- get
	y <- get
	z <- get
	pure $ x + y + z

apply :: Iteratee i a -> [i] -> Maybe a
Pure x `apply` _ = Just x
(Get `Bind` _) `apply` [] = Nothing
(Get `Bind` fa) `apply` (i : is) = (fa `qApp` i) `apply` is

qApp :: MExp (Freer t) a b -> a -> Freer t b
q `qApp` x = case tviewl q of
	TOne (MCont f) -> f x
	MCont f :| r -> case f x of
		Pure y -> r `qApp` y
		tx `Bind` q' -> tx `Bind` (q' >< r)

par :: Iteratee i a -> Iteratee i b -> Iteratee i (Iteratee i a, Iteratee i b)
par l r	| Get `Bind` f <- l, Get `Bind` g <- r = get >>= \x -> par (f `qApp` x) (g `qApp` x)
	| otherwise = pure (l, r)

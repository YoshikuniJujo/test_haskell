{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (
	Freer(..),
	FTCQueue, ViewL(..),
	tsingleton, (|>), (><), tviewl) where

import FTCQueue

data Freer f a = Pure a | forall b . Join (f b) (FTCQueue (Freer f) b a)

instance Functor (Freer f) where
	fmap = (=<<) . (return .)

instance Applicative (Freer f) where
	pure = Pure
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Freer f) where
	return = pure
	Pure x >>= f = f x
	Join fa k >>= f = Join fa $ k |> f

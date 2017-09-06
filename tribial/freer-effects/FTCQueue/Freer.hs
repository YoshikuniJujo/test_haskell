{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (
	Freer(..), FTCQueue, ViewL(..), tsingleton, qApp, qComp ) where

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

qApp :: FTCQueue (Freer t) b w -> b -> Freer t w
qApp q' x = case tviewl q' of
	TOne k -> k x
	k :| t -> case k x of
		Pure y -> qApp t y
		Join u q -> Join u (q >< t)

qComp :: FTCQueue (Freer t1) b w -> (Freer t1 w -> t2) -> b -> t2
qComp g h a = h $ g `qApp` a

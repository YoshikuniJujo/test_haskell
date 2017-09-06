{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (
	Freer(..), FTCQueue, ViewL(..), tsingleton, qApp, qComp ) where

import FTCQueue (FTCQueue, ViewL(..), tsingleton, (|>), (><), tviewl)

data Freer t a = Pure a | forall x . Join (t x) (FTCQueue (Freer t) x a)

instance Functor (Freer t) where
	fmap = (=<<) . (return .)

instance Applicative (Freer t) where
	pure = Pure
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Freer t) where
	Pure x >>= f = f x
	Join m q >>= f = Join m $ q |> f

qApp :: FTCQueue (Freer t) a b -> a -> Freer t b
q `qApp` x = case tviewl q of
	TOne f -> f x
	f :| r -> case f x of
		Pure y -> r `qApp` y
		Join m q' -> Join m (q' >< r)

qComp :: FTCQueue (Freer t) a b -> (Freer t b -> c) -> a -> c
qComp = flip (.) . qApp

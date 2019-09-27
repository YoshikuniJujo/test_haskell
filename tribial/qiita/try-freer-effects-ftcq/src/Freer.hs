{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..), FTCQueue, ViewL(..), tsingleton, qApp, qComp) where

import FTCQueue (FTCQueue, ViewL(..), tsingleton, (|>), (><), tviewl)

data Freer t a = Pure a | forall x . Join (t x) (FTCQueue (Freer t) x a)

instance Functor (Freer t) where
	fmap f (Pure x) = Pure $ f x
	fmap f (Join tx q) = Join tx $ q |> (Pure . f)

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Join tx q <*> m = Join tx $ q |> (<$> m)

instance Monad (Freer t) where
	Pure x >>= f = f x
	Join tx q >>= f = Join tx $ q |> f

qApp :: FTCQueue (Freer t) a b -> a -> Freer t b
q `qApp` x = case tviewl q of
	TOne f -> f x
	f :| r -> case f x of
		Pure y -> r `qApp` y
		Join tx q' -> Join tx (q' >< r)

qComp :: FTCQueue (Freer t) a b -> (Freer t b -> Freer t' c) -> a -> Freer t' c
qComp = flip (.) . qApp

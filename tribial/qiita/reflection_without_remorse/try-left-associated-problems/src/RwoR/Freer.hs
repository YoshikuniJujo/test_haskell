{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RwoR.Freer where

import RwoR.FTCQueue

data Freer t a = Pure a | forall x . t x :>>= FTCQueue (Freer t) x a

instance Functor (Freer t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (tx :>>= q) = tx :>>= (q |> Pure . f)

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx :>>= q <*> m = tx :>>= (q |> (<$> m))

instance Monad (Freer t) where
	Pure x >>= f = f x
	tx :>>= q >>= f = tx :>>= (q |> f)

qApp :: FTCQueue (Freer t) a b -> a -> Freer t b
q `qApp` x = case tviewl q of
	TOne f -> f x
	f :| r -> case f x of
		Pure y -> r `qApp` y
		tx :>>= q' -> tx :>>= (q' >< r)

qComp :: FTCQueue (Freer t) a b -> (Freer t b -> Freer t' c) -> a -> Freer t' c
qComp = flip (.) . qApp

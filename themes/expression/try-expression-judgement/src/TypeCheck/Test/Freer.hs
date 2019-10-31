{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.Freer where

import TypeCheck.Test.MFingerTree

data Freer t a = Pure a | forall x . Bind (t x) (MFingerTree (Freer t) x a)

instance Functor (Freer t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (tx `Bind` q) = tx `Bind` (q |> (Pure . f))

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx `Bind` q <*> m = tx `Bind` (q |> (<$> m))

instance Monad (Freer t) where
	Pure x >>= f = f x
	tx `Bind` q >>= f = tx `Bind` (q |> f)

qApp :: MFingerTree (Freer t) a b -> a -> Freer t b
q `qApp` x = case mviewl q of
	NL -> Pure x
	ConsL (M f) r -> case f x of
		Pure y -> r `qApp` y
		tx `Bind` q' -> tx `Bind` (q' >< r)

qComp :: MFingerTree (Freer t) a b ->
	(Freer t b -> Freer t' c) -> a -> Freer t' c
qComp = flip (.) . qApp

{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import FTCQueue

data Freer t a = Pure a | forall x . t x `Bind` MExp (Freer t) x a

instance Functor (Freer t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (m `Bind` k) = m `Bind` (k |> MCont (pure . f))

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> mx = f <$> mx
	(m `Bind` k) <*> mx = m `Bind` (k |> MCont (<$> mx))

instance Monad (Freer t) where
	Pure x >>= f = f x
	(m `Bind` k) >>= f = m `Bind` (k |> MCont f)

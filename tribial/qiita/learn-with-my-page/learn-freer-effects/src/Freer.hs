{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import Control.Monad ((>=>))

data Freer t a
	= Pure a
	| forall x . Bind (t x) (x -> Freer t a)

instance Functor (Freer t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Bind tx k = Bind tx $ k >=> Pure . f

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Bind tx k <*> m = Bind tx $ k >=> (<$> m)

instance Monad (Freer t) where
	Pure x >>= f = f x
	Bind tx k >>= f = Bind tx $ k >=> f

freer :: t a -> Freer t a
freer = (`Bind` Pure)

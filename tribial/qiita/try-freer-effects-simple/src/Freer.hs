{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..)) where

import Control.Monad ((>=>))

data Freer t a = Pure a | forall x . Bind (t x) (x -> Freer t a)

instance Functor (Freer f) where
	fmap f (Pure x) = Pure $ f x
	fmap f (Bind tx k) = Bind tx $ k >=> Pure . f

instance Applicative (Freer f) where
	pure = Pure
	Pure f <*> m = f <$> m
	Bind tx q <*> m = Bind tx $ q >=> (<$> m)

instance Monad (Freer f) where
	Pure x >>= f = f x
	Bind tx k >>= f = Bind tx $ k >=> f

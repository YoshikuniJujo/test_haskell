{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..)) where

import Control.Monad ((>=>))

data Freer t a = Pure a | forall x . Join (t x) (x -> Freer t a)

instance Functor (Freer f) where
	fmap f (Pure x) = Pure $ f x
	fmap f (Join tx k) = Join tx $ k >=> Pure . f

instance Applicative (Freer f) where
	pure = Pure
	Pure f <*> m = f <$> m
	Join tx q <*> m = Join tx $ q >=> (<$> m)

instance Monad (Freer f) where
	Pure x >>= f = f x
	Join tx k >>= f = Join tx $ k >=> f

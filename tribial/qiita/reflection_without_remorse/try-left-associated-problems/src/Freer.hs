{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..)) where

import Control.Monad ((>=>))

data Freer t a = Pure a | forall x . t x `Bind` (x -> Freer t a)

instance Functor (Freer f) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Bind tx k = tx `Bind` (k >=> Pure . f)

instance Applicative (Freer f) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx `Bind` q <*> m = tx `Bind` (q >=> (<$> m))

instance Monad (Freer f) where
	Pure x >>= f = f x
	tx `Bind` k >>= f = tx `Bind` (k >=> f)

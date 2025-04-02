{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import Control.Monad ((>=>))

data Freer t a
	= Pure a
	| forall x . Bind (t x) (x -> Freer t a)

instance Functor (Freer t) where
	fmap f = \case
		Pure x -> Pure $ f x
		tx `Bind` k -> tx `Bind` (k >=> Pure . f)

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx `Bind` k <*> m = tx `Bind` (k >=> (<$> m))


instance Monad (Freer t) where
	Pure x >>= f = f x
	tx `Bind` k >>= f = tx `Bind` (k >=> f)

freer :: t a -> Freer t a
freer = (`Bind` Pure)

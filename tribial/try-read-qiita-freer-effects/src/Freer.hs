{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import Control.Applicative
import Control.Monad ((>=>), MonadPlus(..))

import NonDetable

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

instance NonDetable t => Alternative (Freer t) where
	empty = mzero; (<|>) = mplus

instance NonDetable t => MonadPlus (Freer t) where
	mzero = mz `Bind` Pure
	m1 `mplus` m2 = mp `Bind` \x -> if x then m1 else m2

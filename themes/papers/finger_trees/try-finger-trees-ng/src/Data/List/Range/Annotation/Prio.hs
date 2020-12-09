{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Annotation.Prio where

import Data.FingerTree.Annotated

data Prio a = MInfty | Prio a deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (Prio a) where
	MInfty <> p = p
	p <> MInfty = p
	Prio m <> Prio n = Prio $ m `max` n

instance (Ord a) => Monoid (Prio a) where mempty = MInfty

instance (Ord a) => Measured (Elem a) (Prio a) where measure (Elem x) = Prio x

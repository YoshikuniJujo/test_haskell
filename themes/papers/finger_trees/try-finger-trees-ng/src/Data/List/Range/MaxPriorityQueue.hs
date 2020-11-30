{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.MaxPriorityQueue where

import Data.List.Range.AnnotatedFingerTree
import Data.View

data Prio a = MInfty | Prio a deriving (Show, Eq, Ord)	

instance (Ord a) => Semigroup (Prio a) where
	MInfty <> p = p
	p <> MInfty = p
	Prio m <> Prio n = Prio $ m `max` n

instance (Ord a) => Monoid (Prio a) where mempty = MInfty

newtype PQueue a = PQueue (FingerTree (Prio a) (Elem a)) deriving Show

instance (Ord a) => Measured (Elem a) (Prio a) where measure (Elem x) = Prio x

extractMax :: Ord a => PQueue a -> (a, PQueue a)
extractMax (PQueue q) = (x, PQueue $ l >< r)
	where
	Split l (Elem x) r = splitTree (measure q <=) mempty q

fromList :: (Ord a, Functor t, Foldable t) => t a -> PQueue a
fromList = PQueue . toTree . (Elem <$>)

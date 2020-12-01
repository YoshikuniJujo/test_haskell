{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.MaxPriorityQueue where

import Data.List.Range.AnnotatedFingerTree
import Data.List.Range.Annotation.Prio
import Data.View

newtype PQueue a = PQueue (FingerTree (Prio a) (Elem a)) deriving Show

extractMax :: Ord a => PQueue a -> (a, PQueue a)
extractMax (PQueue q) = (x, PQueue $ l >< r)
	where
	Split l (Elem x) r = splitTree (measure q <=) mempty q

fromList :: (Ord a, Functor t, Foldable t) => t a -> PQueue a
fromList = PQueue . toTree . (Elem <$>)

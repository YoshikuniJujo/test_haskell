{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.IntervalTree where

import qualified Data.Foldable as F

import Data.FingerTree.Annotated
import Data.List.Range.Annotation.Key
import Data.List.Range.Annotation.Prio
import Data.View

data Interval = Interval { low :: Double, high :: Double } deriving Show

newtype IntervalTree = IntervalTree (FingerTree (Key Double, Prio Double) Interval) deriving Show

instance Measured Interval (Key Double, Prio Double) where
	measure i = (Key $ low i, Prio $ high i)

atleast, greater :: Double -> (Key Double, Prio Double) -> Bool
atleast k (_, h) = Prio k <= h
greater k (l, _) = Key k < l

insert :: Interval -> IntervalTree -> IntervalTree
insert i (IntervalTree xs) = IntervalTree $ l >< (i <| r)
	where (l, r) = split ((>= Key (low i)) . fst) xs

fromList :: Foldable t => t (Double, Double) -> IntervalTree
fromList = foldr (insert . uncurry Interval) $ IntervalTree Empty

toList :: IntervalTree -> [Interval]
toList (IntervalTree xs) = F.toList xs

sampleIntervalList :: [(Double, Double)]
sampleIntervalList = [
	(3, 35),
	(5, 42),
	(15, 100),
	(32, 45),
	(13, 98) ]

intervalSearch :: IntervalTree -> Interval -> Maybe Interval
intervalSearch (IntervalTree t) i
	| atleast (low i) (measure t) && low x <= high i = Just x
	| otherwise = Nothing
	where Split _ x _ = splitTree (atleast $ low i) mempty t

intervalMatch :: IntervalTree -> Interval -> [Interval]
intervalMatch (IntervalTree t) i = matches (takeUntil (greater $ high i) t)
	where matches xs = case viewL (dropUntil (atleast $ low i) xs) of
		NL -> [] :: [Interval]
		ConsL x xs' -> x : matches xs'

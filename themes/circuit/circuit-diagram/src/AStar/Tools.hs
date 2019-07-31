{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar.Tools where

import Data.Map.Strict
import Data.Heap

doUntil :: Monad m => m (Maybe a) -> m a
doUntil act = do
	mx <- act
	maybe (doUntil act) return mx

toRoute :: Ord n => [n] -> n -> Map n n -> [n]
toRoute ns n m = case m !? n of
	Just n' -> toRoute (n : ns) n' m
	Nothing -> n : ns

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe (Just x) (Just y) = Just $ min x y
minMaybe (Just x) Nothing = Just x
minMaybe Nothing (Just y) = Just y
minMaybe Nothing Nothing = Nothing

headHeap :: Heap a -> Maybe a
headHeap = (fst <$>) . uncons

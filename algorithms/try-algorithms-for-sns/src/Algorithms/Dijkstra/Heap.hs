{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra.Heap where

import Control.Arrow
import Data.Function
import Data.Maybe
import Data.List qualified as L
import Data.Array.IArray
import Algorithms.Heap.Skew qualified as H

-- DIJKSTRA'S ALGORITHM

dijkstra :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> n -> n -> Maybe d
dijkstra r g s = solve r [] (singleton (s, 0)) (loadGraph r g)

solve :: (Ix n, Ord d, Num d) =>
	(n, n) -> [n] -> Queue n d -> Graph n d -> n -> Maybe d
solve r fx q g t = case dequeue q of
	Nothing -> Nothing
	Just ((x, d), q')
		| x `elem` fx -> solve r fx q' g t
		| x == t -> Just d
		| otherwise -> solve r (x : fx) (foldr (upd d) q' (g ! x)) g t
	where upd d = enqueue . ((d +) `second`)

type Queue n d = H.Heap (NodeDist n d)
type Graph n d = Array n [(n, d)]

-- QUEUE

data NodeDist n d = NodeDist n d deriving (Eq, Show)

instance (Ord n, Ord d) => Ord (NodeDist n d) where
	NodeDist n d <= NodeDist m e = d < e || d == e && n <= m

singleton :: (n, d) -> Queue n d
singleton = H.singleton . uncurry NodeDist

enqueue :: (Ord n, Ord d) => (n, d) -> Queue n d -> Queue n d
enqueue (n, d) = (`H.insert` NodeDist n d)

dequeue :: (Ord n, Ord d) => Queue n d -> Maybe ((n, d), Queue n d)
dequeue = (((\(NodeDist n d) -> (n, d)) `first`) <$>) . H.deleteMin

-- LOAD GRAPH

loadGraph :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> Graph n d
loadGraph r = array r . (out `mapMaybe`) . pp
	where
	out = \case
		[] -> Nothing
		g@(((n, _), _) : _) -> Just (n, (\((_, m), d) -> (m, d)) <$> g)
	pp g = L.groupBy ((==) `on` fst . fst) . L.sort $ concat [
		(\n -> ((n, n), 0)) <$> range r, g,
		(first \(x, y) -> (y, x)) <$> g ]

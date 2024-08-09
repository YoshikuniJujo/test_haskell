{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra.Heap where

import Control.Arrow
import Data.Function
import Data.List qualified as L
import Data.Array.IArray

-- DIJKSTRA'S ALGORITHM

dijkstra' :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> n -> n -> Maybe d
dijkstra' r g s = solve' r [] [(s, 0)] (loadGraph2 r g)

solve' :: (Ix n, Ord d, Num d) =>
	(n, n) -> [n] -> [(n, d)] -> Graph' n d -> n -> Maybe d
solve' r fx q g t = case dequeue q of
	Nothing -> Nothing
	Just ((x, d), q')
		| x `elem` fx -> solve' r fx q' g t
		| x == t -> Just d
		| otherwise ->
			solve' r (x : fx) (foldr (update' d) q' (g ! x)) g t

update' :: Num d => d -> (n, d) -> [(n, d)] -> [(n, d)]
update' dx (y, dxy) = enqueue (y , dx + dxy)

type Graph' n d = Array n [(n, d)]

enqueue :: (n, d) -> [(n, d)] -> [(n, d)]
enqueue = (:)

dequeue :: Ord d => [(n, d)] -> Maybe ((n, d), [(n, d)])
dequeue = \case [] -> Nothing; h : t -> Just $ go h t
	where go x@(_, mn) = \case
		[] -> (x, [])
		y@(_, d) : q
			| mn <= d -> (y :) `second` go x q
			| otherwise -> (x :) `second` go y q

-- LOAD GRAPH

loadGraph2 :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> Graph' n d
loadGraph2 r = array r . preprocessGraph r

preprocessGraph :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> [(n, [(n, d)])]
preprocessGraph r g = (factorOut <$>) . L.groupBy ((==) `on` fst . fst) . L.sort $ concat [
	(\n -> ((n, n), 0)) <$> range r,
	g, (\((n1, n2), d) -> ((n2, n1), d)) <$> g ]

factorOut :: [((n, n), d)] -> (n, [(n, d)])
factorOut [] = error "bad"
factorOut g@(((n0, _), _) : _) = (n0, (\((_, n), d) -> (n, d)) <$> g)

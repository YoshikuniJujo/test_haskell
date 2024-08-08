{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST

-- DIJKSTRA'S ALGORITHM

dijkstra :: (Ix n, Ord d, Num d) => (n, n) -> [((n, n), d)] -> n -> n -> Maybe d
dijkstra r g s = solve r [] [(s, 0)] (loadGraph r g)

solve :: (Ix n, Ord d, Num d) =>
	(n, n) -> [n] -> [(n, d)] -> Array2d n (Maybe d) -> n -> Maybe d
solve r fx q g t = case dequeue q of
	Nothing -> Nothing
	Just ((x, d), q')
		| x `elem` fx -> solve r fx q' g t
		| x == t -> Just d
		| otherwise ->
			solve r (x : fx) (foldr (update g x d) q' (range r)) g t

update :: (Ix n, Num d) =>
	Array2d n (Maybe d) -> n -> d -> n -> [(n, d)] -> [(n, d)]
update g x d y = maybe id (enqueue . (y ,) . (d +)) $ g ! x ! y

type Array2d n e = Array n (Array n e)

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

loadGraph :: forall n w . (Ix n, Num w) =>
	(n, n) -> [((n, n), w)] -> Array n (Array n (Maybe w))
loadGraph rng edgs = runST $ loadGraph' rng edgs

loadGraph' :: forall s n w . (Num w, Ix n) =>
	(n, n) -> [((n, n), w)] -> ST s (Array n (Array n (Maybe w)))
loadGraph' rng edgs = (freeze `mapM`) =<< do
	a <- newSquareArray @(STArray s) rng Nothing
	a <$ do	for_ (range rng) \i -> writeArray (a ! i) i $ Just 0
		for_ edgs \((n1, n2), d) ->
			writeArray (a ! n1) n2 (Just d) >>
			writeArray (a ! n2) n1 (Just d)

newSquareArray ::
	(MArray a e m, IArray b (a i e), Ix i) => (i, i) -> e -> m (b i (a i e))
newSquareArray r d = listArray r <$> replicateM (rangeSize r) (newArray r d)

-- SAMPLES

-- > dijkstra (A, H) sample1 A H

sample1 :: [((Node, Node), Word)]
sample1 = [
	((A, B), 1), ((A, C), 7), ((A, D), 2),
	((B, E), 2), ((B, F), 4),
	((C, F), 2), ((C, G), 3),
	((D, G), 5), ((E, F), 1), ((F, H), 6), ((G, H), 2) ]

data Node = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord, Ix)

-- > dijkstra (1, 6) sample2 1 6

sample2 :: [((Node2, Node2), Word)]
sample2 = [
	((1, 2), 7), ((1, 3), 9), ((1, 6), 14),
	((2, 3), 10), ((2, 4), 15),
	((3, 4), 11), ((3, 6), 2),
	((4, 5), 6), ((5, 6), 9) ]

type Node2 = Word

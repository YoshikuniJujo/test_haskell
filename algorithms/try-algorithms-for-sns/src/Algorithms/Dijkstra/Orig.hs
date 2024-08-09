{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra.Orig where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Bool

-- DIJKSTRA'S ALGORITHM

dijkstra :: (Ix n, Ord d, Bounded d, Num d) =>
	(n, n) -> [((n, n), d)] -> n -> n -> Maybe d
dijkstra r g s t = runST do
	(fx, q) <- (,) <$> newArray r False <*> newArray r maxBound
	enqueue q s 0 >> solve r fx q (loadGraph r g) t

solve :: (Ix n, Ord d, Num d) =>
	(n, n) -> Fixed s n -> Queue s n d -> Graph n d -> n -> ST s (Maybe d)
solve r fx q g t = dequeue fx q >>= \case
	Nothing -> pure Nothing
	Just (x, d)
		| x == t -> pure $ Just d
		| otherwise -> upd x d `mapM_` range r >> solve r fx q g t
	where upd x d y = maybe (pure ()) (enqueue q y . (d +)) $ g ! x ! y

type Fixed s n = STArray s n Bool
type Queue s n d = STArray s n d
type Graph n d = Array n (Array n (Maybe d))

enqueue :: (Ix n, Ord d) => Queue s n d -> n -> d -> ST s ()
enqueue q n d = readArray q n >>= \d0 -> when (d < d0) $ writeArray q n d

dequeue :: (Ix n, Ord d) => Fixed s n -> Queue s n d -> ST s (Maybe (n, d))
dequeue fx q = range <$> getBounds q >>= \ns ->
	filterM ((not <$>) . readArray fx) ns >>= \case
		[] -> pure Nothing
		n : ns' -> Just <$> do
			r@(fn, _) <- flip (go n) ns' =<< readArray q n
			r <$ writeArray fx fn True
	where
	go n0 d0 [] = pure (n0, d0)
	go n0 d0 (n : ns) = readArray q n >>= \d ->
		bool (go n d ns) (go n0 d0 ns) (d0 <= d)

-- LOAD GRAPH

loadGraph :: forall n d . (Ix n, Num d) => (n, n) -> [((n, n), d)] -> Graph n d
loadGraph rng edgs = runST $ loadGraph' rng edgs

loadGraph' :: forall s n d . (Num d, Ix n) =>
	(n, n) -> [((n, n), d)] -> ST s (Graph n d)
loadGraph' rng edgs = (freeze `mapM`) =<< do
	a <- newSquareArray @(STArray s) rng Nothing
	a <$ do	for_ (range rng) \i -> writeArray (a ! i) i $ Just 0
		for_ edgs \((n1, n2), d) ->
			writeArray (a ! n1) n2 (Just d) >>
			writeArray (a ! n2) n1 (Just d)

newSquareArray ::
	(MArray a e m, IArray b (a i e), Ix i) => (i, i) -> e -> m (b i (a i e))
newSquareArray r d = listArray r <$> replicateM (rangeSize r) (newArray r d)

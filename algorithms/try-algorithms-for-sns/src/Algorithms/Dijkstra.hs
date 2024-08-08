{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.List qualified as L
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST

data Node = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord, Ix)

sampleNode :: [((Node, Node), Word)]
sampleNode = [
	((A, B), 1), ((A, C), 7), ((A, D), 2),
	((B, E), 2), ((B, F), 4),
	((C, F), 2), ((C, G), 3),
	((D, G), 5), ((E, F), 1), ((F, H), 6), ((G, H), 2) ]

newSquareArray ::
	(MArray a e m, IArray b (a i e), Ix i) => (i, i) -> e -> m (b i (a i e))
newSquareArray r d = do
	as <- replicateM (rangeSize r) $ newArray r d
	pure $ listArray r as

freeze2d :: (
	MArray a e m, IArray b e,
	MArray a (a i e) m, IArray b (a i e),
	Traversable (b i), Ix i ) =>
	a i (a i e) -> m (b i (b i e))
freeze2d ma = (freeze `mapM`) =<< freeze ma

loadGraph :: forall n w . (Ix n, Num w, Bounded w) =>
	(n, n) -> [((n, n), w)] -> Array n (Array n w)
loadGraph rng edgs = runST $ loadGraph' rng edgs

loadGraph' :: forall s n w . (Num w, Ix n, Bounded w, MArray (STArray s) w (ST s)) =>
	(n, n) -> [((n, n), w)] -> ST s (Array n (Array n w))
loadGraph' rng edgs = (freeze `mapM`) =<< do
	a <- newSquareArray @(STArray s) rng 1000000 -- maxBound
	for_ (range rng) \i -> writeArray (a ! i) i 0
	for_ edgs \((n1, n2), d) ->
		writeArray (a ! n1) n2 d >> writeArray (a ! n2) n1 d
	pure a

-- type Shortests s n w = STArray s n w
type Confirms s n = STArray s n Bool

enqueue :: Ord a => (n, a) -> [(n, a)] -> [(n, a)]
enqueue x [] = [x]
enqueue x@(_, dx) xa@(y@(_, dy) : xs)
	| dx <= dy = x : xa
	| otherwise = y : enqueue x xs

{-
dequeue :: [a] -> Maybe a
dequeue = \case [] -> Nothing; x : _ -> Just x
-}

dequeue :: [a] -> (a, [a])
dequeue = fromJust . L.uncons

solve :: (Ix n, Num w, Ord w) => [n] -> [(n, w)] -> Array n (Array n w) -> n -> w
solve fixed q g t
	| x `elem` fixed = solve fixed q' g t
	| x == t = w
	| otherwise = solve
		(x : fixed)
		(foldr (\y -> enqueue (y, w + g ! x ! y)) q' (indices $ g ! x))
		g t
	where ((x, w), q') = dequeue q

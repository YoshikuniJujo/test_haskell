{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra.Samples where

import Data.Ix

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

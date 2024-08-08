{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Dijkstra where

import Data.Ix

data Node = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord, Ix)

sampleNode :: [((Node, Node), Word)]
sampleNode = [
	((A, B), 1), ((A, C), 7), ((A, D), 2),
	((B, E), 2), ((B, F), 4),
	((C, F), 2), ((C, G), 3),
	((D, G), 5), ((E, F), 1), ((F, H), 6), ((G, H), 2) ]

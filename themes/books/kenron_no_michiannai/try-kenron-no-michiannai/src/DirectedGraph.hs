{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DirectedGraph where

class DirectedGraph a n where
	start :: a -> n
	end :: a -> n

data Arrows = Arrow1 | Arrow2 | Arrow3 deriving Show

data Nodes = A | B | C | D deriving Show

instance DirectedGraph Arrows Nodes where
	start = \case
		Arrow1 -> A
		Arrow2 -> B
		Arrow3 -> C
	end = \case
		Arrow1 -> B
		Arrow2 -> C
		Arrow3 -> D

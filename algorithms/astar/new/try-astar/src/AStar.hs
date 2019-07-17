{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar where

import AStarMonad

class AStar a where
	type AStarNode a
	startNode :: a -> AStarNode a
	isEndNode :: a -> AStarNode a -> Bool
	nextNodes :: a -> AStarNode a -> [(AStarNode a, Dist)]
	distToEnd :: a -> AStarNode a -> Dist

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar.AStar (AStar(..), Dist, astar) where

import Data.Foldable

import AStar.AStarMonad
import AStar.Tools

class AStar a where
	type AStarNode a
	startNode :: a -> AStarNode a
	isEndNode :: a -> AStarNode a -> Bool
	nextNodes :: a -> AStarNode a -> [(AStarNode a, Dist)]
	distToEnd :: a -> AStarNode a -> Dist

astar :: (AStar a, Ord (AStarNode a)) => a -> Maybe [AStarNode a]
astar a = do
	(g, (_, m)) <- runAStarM
		$ putNode Open (distToEnd a s) s >> doUntil (step a)
	return $ toRoute [] g m
	where s = startNode a

step :: (AStar a, Ord (AStarNode a)) =>
	a -> AStarM (AStarNode a) (Maybe (AStarNode a))
step a = do
	(pd, pn) <- headNode
	if isEndNode a pn then return $ Just pn else do
		putNode Close pd pn
		for_ (nextNodes a pn) $ \(cn, cdte) -> do
			let	cd = pd - distToEnd a pn + cdte + distToEnd a cn
			putOpen cd cn pn
		return Nothing

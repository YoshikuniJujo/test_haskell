{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStarNode where

import Data.Foldable

import AStarMonad
import Tools

class GlaphNode n where
	startNode :: n
	isEndNode :: n -> Bool
	nextNodes :: n -> [(n, Dist)]
	distToEnd :: n -> Dist

astar :: forall n . (GlaphNode n, Ord n) => Maybe [n]
astar = do
	(g, (_, m)) <- runAStarM $ putNode Open (distToEnd s) s >> doUntil step
	return $ toRoute [] g m
	where s = startNode :: n

step :: (GlaphNode n, Ord n) => AStarM n (Maybe n)
step = do
	(pd, pn) <- headNode
	if isEndNode pn then return $ Just pn else do
		putNode Close pd pn
		for_ (nextNodes pn) $ \(cn, cdte) -> do
			let	cd = pd - distToEnd pn + cdte + distToEnd cn
			putOpen cd cn pn
		return Nothing

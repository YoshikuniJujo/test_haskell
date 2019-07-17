{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Scraps.AStar where

import Control.Monad.State
import Data.Foldable
import Data.Map.Strict
import Data.Heap

import qualified Data.Map as M
import qualified Data.Heap as H

class GlaphNode n where
	startNode :: n
	isEndNode :: n -> Bool
	nextNodes :: n -> [(n, Word)]
	distToEnd :: n -> Word

	fromNodeId :: NodeId -> n
	toNodeId :: n -> NodeId

data Moment = Moment {
	nodeOpen :: Heap (Tag, Word, NodeId),
	nodeParent :: Map NodeId NodeId }
	deriving Show

data Tag = Open | Close deriving (Show, Eq, Ord)
newtype NodeId = NodeId Word deriving (Show, Eq, Ord)

type AStarMonad = StateT Moment Maybe

step :: forall n . GlaphNode n => AStarMonad (Maybe NodeId)
step = do
	(dst, nid) <- headNode
	let	nd :: n = fromNodeId nid
	if isEndNode nd then return $ Just nid else do
		closeNode dst nid
		let	nns = nextNodes nd
		for_ nns $ \(n, d) -> do
			let	f' = dst - distToEnd nd + d + distToEnd n
				nid' = toNodeId n
			openNode f' nid'
			m@Moment { nodeParent = np } <- get
			put m { nodeParent = M.insert nid' nid np }
		return Nothing

headNode :: AStarMonad (Word, NodeId)
headNode = do
	m@Moment { nodeOpen = no } <- get
	((tg, dst, nid), no') <- lift $ uncons no
	put m { nodeOpen = no' }
	case tg of
		Open -> return (dst, nid)
		Close -> lift Nothing

openNode :: Word -> NodeId -> AStarMonad ()
openNode dst nd = do
	m@Moment { nodeOpen = no } <- get
	put m { nodeOpen = H.insert (Open, dst, nd) no }

closeNode :: Word -> NodeId -> AStarMonad ()
closeNode dst nd = do
	m@Moment { nodeOpen = no } <- get
	put m { nodeOpen = H.insert (Close, dst, nd) no }

sampleMoment :: Moment
sampleMoment = Moment {
	nodeOpen = sampleOpenList,
	nodeParent = M.empty }

sampleOpenList :: Heap (Tag, Word, NodeId)
sampleOpenList = H.fromList [
	(Close, 123, NodeId 3),
	(Open, 321, NodeId 4) ]

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStarNode where

import Prelude as P

import Control.Monad.State
import Data.Foldable
import Data.Map.Strict
import Data.Heap

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Foldable as F

type Dist = Word

class GlaphNode n where
	startNode :: n
	isEndNode :: n -> Bool
	nextNodes :: n -> [(n, Dist)]
	distToEnd :: n -> Dist

data Tag = Open | Close deriving (Show, Eq, Ord)

type Moment n = (Heap (Tag, Dist, n), Map n n)

type AStarMonad n = StateT (Moment n) Maybe

doUntil :: Monad m => m (Maybe a) -> m a
doUntil act = do
	mx <- act
	maybe (doUntil act) return mx

astar :: forall n . (GlaphNode n, Ord n) => Maybe [n]
astar = do
	(g, (_, m)) <-
		(putNode Open (distToEnd (startNode :: n)) startNode >> doUntil step)
			`runStateT` (H.empty, M.empty)
	return $ toRoute [] g m

toRoute :: Ord n => [n] -> n -> Map n n -> [n]
toRoute ns n m = case m !? n of
	Just n' -> toRoute (n : ns) n' m
	Nothing -> n : ns

step :: (GlaphNode n, Ord n) => AStarMonad n (Maybe n)
step = do
	(dst, nd) <- headNode
	if isEndNode nd then return $ Just nd else do
		putNode Close dst nd
		for_ (nextNodes nd) $ \(n, d) -> do
			let	f' = dst - distToEnd nd + d + distToEnd n
			putOpen f' n nd
		return Nothing

headNode :: AStarMonad n (Dist, n)
headNode = do
	(h, m) <- get
	((tg, dst, n), h') <- lift $ uncons h
	put (h', m)
	case tg of
		Open -> return (dst, n)
		Close -> lift Nothing

putNode :: Ord n => Tag -> Dist -> n -> AStarMonad n ()
putNode t d n = do
	(h, m) <- get
	put (H.insert (t, d, n) h, m)

setParent :: Ord n => n -> n -> AStarMonad n ()
setParent c p = do
	(h, m) <- get
	put (h, M.insert c p m)

putOpen :: Ord n => Dist -> n -> n -> AStarMonad n ()
putOpen d n nd = do
	h <- gets fst
	case (\(_, d'', _) -> d'') <$> F.toList (H.filter ((== n) . (\(_, _, n') -> n')) h) of
		[] -> do
			putNode Open d n
			setParent n nd
		ds -> let d' = P.minimum ds in
			if d < d' then putNode Open d n >> setParent n nd else return ()

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar.AStarMonad (
	AStarM, runAStarM, headNode, putNode, putOpen, Dist, Switch(..) ) where

import Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Map.Strict
import Data.Heap

import qualified Data.Map as M
import qualified Data.Heap as H

import AStar.Tools

data Switch = Open | Close deriving (Show, Eq, Ord)

switch :: a -> a -> Switch -> a
switch x _ Open = x
switch _ y Close = y

type Dist = Word

type Node n = (Switch, (Dist, n))

tag :: Node n -> Switch
tag = fst

dist :: Node n -> Dist
dist = fst . snd

node :: Node n -> n
node = snd . snd

type AStarM n = StateT (Moment n) Maybe
type Moment n = (Heap (Node n), Map n n)

runAStarM :: AStarM n a -> Maybe (a, Moment n)
runAStarM = (`runStateT` (H.empty, M.empty))

putOpen :: Ord n => Dist -> n -> n -> AStarM n ()
putOpen d n pr = do
	ms <- shortest n <$> gets fst
	case ms of
		Just s | s <= d -> return ()
		_ -> putNode Open d n >> setParent n pr

shortest :: Eq n => n -> Heap (Switch, (Dist, n)) -> Maybe Dist
shortest n = uncurry minMaybe
	. ((dist <$>) . headHeap *** (dist <$>) . headHeap)
	. H.partition ((== Open) . tag) . H.filter ((== n) . node)

headNode :: AStarM n (Dist, n)
headNode = do
	(h, m) <- get
	((tg, dn), h') <- lift $ uncons h
	put (h', m)
	switch (return dn) (lift Nothing) tg

putNode :: Ord n => Switch -> Dist -> n -> AStarM n ()
putNode t d n = modify . first $ H.insert (t, (d, n))

setParent :: Ord n => n -> n -> AStarM n ()
setParent c p = modify . second $ M.insert c p

{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.FTCQueue (FTCQueue) where

import Control.Monad.Freer.Par.Sequence

data FTCQueue cat a b where
	Empty :: FTCQueue cat a a
	Node :: FTCQueue cat a b ->
		cat b c -> FTCQueue cat c d -> FTCQueue cat a d

instance Sequence FTCQueue where
	empty = Empty
	singleton x = Node Empty x Empty
	l >< r = case fviewl r of EmptyL -> l; x :<| r' -> Node l x r'
	viewl = fviewl

fviewl :: FTCQueue c a b -> ViewL FTCQueue c a b
fviewl Empty = EmptyL
fviewl (Node l0 x0 r0) = vwl l0 x0 r0
	where
	vwl :: FTCQueue cat a b ->
		cat b c -> FTCQueue cat c d -> ViewL FTCQueue cat a d
	vwl Empty x r = x :<| r
	vwl (Node ll x lr) y r = vwl ll x $ Node lr y r

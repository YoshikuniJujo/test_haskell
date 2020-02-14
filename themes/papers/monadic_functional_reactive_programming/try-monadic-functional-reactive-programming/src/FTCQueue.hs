{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue (FTCQueue) where

import Sequence

data FTCQueue c a b where
	Empty :: FTCQueue c a a
	Tree :: Tree c a b -> FTCQueue c a b

data Tree c a b = Leaf (c a b) | forall x . Tree c a x :>< Tree c x b

instance Sequence FTCQueue where
	empty = Empty
	singleton = Tree . Leaf
	Empty >< t = t
	t >< Empty = t
	Tree t >< Tree t' = Tree $ t :>< t'
	viewl = fviewl

fviewl :: FTCQueue c a b -> ViewL FTCQueue c a b
fviewl Empty = EmptyL
fviewl (Tree (Leaf x)) = x :<| Empty
fviewl (Tree (l0 :>< r0)) = l0 `go` r0
	where
	go :: Tree c a x -> Tree c x b -> ViewL FTCQueue c a b
	Leaf x `go` r = x :<| Tree r
	(ll :>< lr) `go` r = ll `go` (lr :>< r)

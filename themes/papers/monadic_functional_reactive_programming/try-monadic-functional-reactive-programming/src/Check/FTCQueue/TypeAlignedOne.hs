{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.FTCQueue.TypeAlignedOne where

data FTCQueue cat a b
	= Leaf (cat a b) | forall x . FTCQueue cat a x :>< FTCQueue cat x b

data ViewL sq cat a b = OneL (cat a b) | forall x . cat a x :| sq cat x b

viewl :: FTCQueue cat a b -> ViewL FTCQueue cat a b
viewl (Leaf x) = OneL x
viewl (l0 :>< r0) = l0 `go` r0
	where
	go :: FTCQueue cat a x -> FTCQueue cat x b -> ViewL FTCQueue cat a b
	Leaf x `go` r = x :| r
	(ll :>< lr) `go` r = ll `go` (lr :>< r)

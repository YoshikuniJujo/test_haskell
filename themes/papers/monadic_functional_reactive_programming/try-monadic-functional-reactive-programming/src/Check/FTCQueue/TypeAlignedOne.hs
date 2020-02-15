{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.FTCQueue.TypeAlignedOne where

data FTCQueue c a b
	= Leaf (c a b) | forall x . FTCQueue c a x :>< FTCQueue c x b

data ViewL sq c a b = OneL (c a b) | forall x . c a x :| sq c x b

{-
viewl :: FTCQueue a -> Either a (a, FTCQueue a)
viewl (Leaf x) = Left x
viewl (l0 :>< r0) = l0 `go` r0
	where
	Leaf x `go` r = Right (x, r)
	(ll :>< lr) `go` r = ll `go` (lr :>< r)
	-}

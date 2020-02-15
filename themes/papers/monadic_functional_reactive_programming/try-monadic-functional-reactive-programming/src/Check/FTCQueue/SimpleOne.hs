{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.FTCQueue.SimpleOne where

data FTCQueue a = Leaf a | FTCQueue a :>< FTCQueue a deriving Show

viewl :: FTCQueue a -> Either a (a, FTCQueue a)
viewl (Leaf x) = Left x
viewl (l0 :>< r0) = l0 `go` r0
	where
	Leaf x `go` r = Right (x, r)
	(ll :>< lr) `go` r = ll `go` (lr :>< r)

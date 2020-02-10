{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue (
	MFTCQueue, MCont(..),
	FTCQueue, tsingleton, (><), (|>), tviewl, ViewL(..) ) where

data FTCQueue c a b
	= Leaf (c a b)
	| forall x . FTCQueue c a x :>< FTCQueue c x b

tsingleton :: c a b -> FTCQueue c a b
tsingleton = Leaf

(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(><) = (:><)

(|>) :: FTCQueue c a x -> c x b -> FTCQueue c a b
(|>) t = (t :><) . tsingleton

data ViewL c a b
	= TOne (c a b)
	| forall x . c a x :| FTCQueue c x b

tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf f) = TOne f
tviewl (l0 :>< r0) = go l0 r0
	where
	go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
	go (Leaf f) r = f :| r
	go (ll :>< lr) r = go ll (lr :>< r)

newtype MCont m a b = MCont (a -> m b)
type MFTCQueue m a b = FTCQueue (MCont m) a b

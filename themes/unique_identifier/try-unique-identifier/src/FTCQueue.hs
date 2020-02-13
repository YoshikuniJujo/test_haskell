{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue (
	MExp, MCont(..),
	TaggedExp, Tagged(..),
	EitherTagExp, EitherTag(..),
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

tviewl :: FTCQueue c a b -> ViewL c a b
tviewl (Leaf f) = TOne f
tviewl (l0 :>< r0) = l0 `go` r0
	where
	go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
	Leaf f `go` r = f :| r
	(ll :>< lr) `go` r = ll `go` (lr :>< r)

newtype MCont m a b = MCont (a -> m b)
type MExp m a b = FTCQueue (MCont m) a b

data Tagged s m a b = Tagged Integer (a -> m b)
type TaggedExp s m a b = FTCQueue (Tagged s m) a b

data EitherTag s m a b where
	Open :: Integer -> EitherTag s m a a
	Close :: Integer -> EitherTag s m a a
	Fun :: (a -> m b) -> EitherTag s m a b
type EitherTagExp s m a b = FTCQueue (EitherTag s m) a b

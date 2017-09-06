{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue (
	FTCQueue, ViewL(..), tsingleton, (|>), (><), tviewl ) where

data FTCQueue m a b
	= Leaf (a -> m b)
	| forall x . Node (FTCQueue m a x) (FTCQueue m x b)

tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
(|>) = (. Leaf) . Node

(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(><) = Node

data ViewL m a b = TOne (a -> m b) | forall x . (a -> m x) :| FTCQueue m x b

tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf f) = TOne f
tviewl (Node l0 r0) = go l0 r0
	where
	go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
	go (Leaf f) r = f :| r
	go (Node ll lr) r = go ll (Node lr r)

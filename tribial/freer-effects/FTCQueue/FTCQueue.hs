{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue (
	FTCQueue, ViewL(..), tsingleton, (|>), (><), tviewl ) where

data FTCQueue t a b
	= Leaf (a -> t b)
	| forall x . Node (FTCQueue t a x) (FTCQueue t x b)

tsingleton :: (a -> t b) -> FTCQueue t a b
tsingleton = Leaf

(|>) :: FTCQueue t a x -> (x -> t b) -> FTCQueue t a b
(|>) = (. Leaf) . Node

(><) :: FTCQueue t a x -> FTCQueue t x b -> FTCQueue t a b
(><) = Node

data ViewL t a b = TOne (a -> t b) | forall x . (a -> t x) :| FTCQueue t x b

tviewl :: FTCQueue t a b -> ViewL t a b
tviewl (Leaf f) = TOne f
tviewl (Node l0 r0) = go l0 r0
	where
	go :: FTCQueue t a x -> FTCQueue t x b -> ViewL t a b
	go (Leaf f) r = f :| r
	go (Node ll lr) r = go ll (Node lr r)

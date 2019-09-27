{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FunctionTree where

data FunctionTree a b
	= Leaf (a -> b)
	| forall x . Node (FunctionTree a x) (FunctionTree x b)

tsingleton :: (a -> b) -> FunctionTree a b
tsingleton = Leaf

(|>) :: FunctionTree a b -> (b -> c) -> FunctionTree a c
(|>) = (. Leaf) . Node

(>|) :: (a -> b) -> FunctionTree b c -> FunctionTree a c
(>|) = Node . Leaf

(><) :: FunctionTree a b -> FunctionTree b c -> FunctionTree a c
(><) = Node

data ViewL a b = TOne (a -> b) | forall x . (a -> x) :| FunctionTree x b

tviewl :: FunctionTree a b -> ViewL a b
tviewl (Leaf f) = TOne f
tviewl (Node l0 r0) = go l0 r0
	where
	go :: FunctionTree a x -> FunctionTree x b -> ViewL a b
	go (Leaf f) r = f :| r
	go (Node ll lr) r = go ll (Node lr r)

ftApp :: FunctionTree a b -> a -> b
ft `ftApp` x = case tviewl ft of
	TOne f -> f x
	f :| r -> r `ftApp` f x

ftComp :: FunctionTree a b -> (b -> c) -> a -> c
ftComp = flip (.) . ftApp

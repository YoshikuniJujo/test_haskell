{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQueue.FTCQueue (Q, ViewL(..), singleton, (|>), (><), viewl) where

data Q t a b = Leaf (a -> t b) | forall x . Node (Q t a x) (Q t x b)

singleton :: (a -> t b) -> Q t a b
singleton = Leaf

(|>) :: Q t a b -> (b -> t c) -> Q t a c
(|>) = (. Leaf) . Node

(><) :: Q t a b -> Q t b c -> Q t a c
(><) = Node

data ViewL t a b = One (a -> t b) | forall x . (a -> t x) :| Q t x b

viewl :: Q t a b -> ViewL t a b
viewl (Leaf f) = One f
viewl (Node l0 r0) = go l0 r0
	where
	go :: Q t a x -> Q t x b -> ViewL t a b
	go (Leaf f) r = f :| r
	go (Node ll lr) r = go ll (Node lr r)

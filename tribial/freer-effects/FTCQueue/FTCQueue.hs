{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue where

data FTCQueue m a b
	= Leaf (a -> m b)
	| forall x . Node (FTCQueue m a x) (FTCQueue m x b)

tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)

(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
t1 >< t2 = Node t1 t2

data ViewL m a b
	= TOne (a -> m b)
	| forall x . (a -> m x) :| FTCQueue m x b

tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
	where
	go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
	go (Leaf r) tr = r :| tr
	go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

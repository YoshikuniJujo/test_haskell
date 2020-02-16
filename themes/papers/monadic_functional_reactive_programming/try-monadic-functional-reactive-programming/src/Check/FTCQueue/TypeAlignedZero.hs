{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.FTCQueue.TypeAlignedZero where

data FTCQueue cat a b where
	Empty :: FTCQueue cat a a
	Node :: FTCQueue cat a b -> cat b c -> FTCQueue cat c d -> FTCQueue cat a d

data ViewL sq cat a b = EmptyL | forall x . cat a x :| sq cat x b

viewl :: FTCQueue cat a b -> ViewL FTCQueue cat a b
viewl Empty = EmptyL
viewl (Node l0 x0 r0) = go l0 x0 r0
	where
	go :: FTCQueue cat a b -> cat b c -> FTCQueue cat c d -> ViewL FTCQueue cat a d
	go Empty x r = x :| r
	go (Node ll x lr) y r = go ll x (Node lr y r)

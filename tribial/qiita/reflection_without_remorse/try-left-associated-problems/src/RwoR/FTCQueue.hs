{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RwoR.FTCQueue where

data FTCQueue t a b
	= Leaf (a -> t b)
	| forall x . Node (FTCQueue t a x) (FTCQueue t x b)

-- data MoreGeneral c a b
-- 	= Leaf (c a b)
-- 	| forall x . Node (MoreGeneral c a x) (MoreGeneral c x b)
--
-- type MonadCat m a b = a -> m b
-- type FTCQueue t a b = MoreGeneral (MonadCat t) a b

tsingleton :: (a -> t b) -> FTCQueue t a b
tsingleton = Leaf

(|>) :: FTCQueue t a b -> (b -> t c) -> FTCQueue t a c
(|>) = (. Leaf) . Node

(><) :: FTCQueue t a b -> FTCQueue t b c -> FTCQueue t a c
(><) = Node

data ViewL t a b
	= TOne (a -> t b)
	| forall x . (a -> t x) :| FTCQueue t x b

-- data MoreGeneralViewL c a b
-- 	= TOne (c a b)
-- 	| forall x . c a x :| FTCQueue t x b
-- type ViewL t a b = MoreGeneralViewL (MonadCat t) a b

tviewl :: FTCQueue t a b -> ViewL t a b
tviewl (Leaf f) = TOne f
tviewl (Node l0 r0) = go l0 r0
	where
	go :: FTCQueue t a x -> FTCQueue t x b -> ViewL t a b
	go (Leaf f) r = f :| r
	go (Node ll lr) r = go ll (Node lr r)

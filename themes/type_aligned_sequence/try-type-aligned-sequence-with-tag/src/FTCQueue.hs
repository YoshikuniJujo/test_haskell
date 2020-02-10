{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FTCQueue where

data FTCQueue m a b
	= Leaf (a -> m b)
	| forall x . Node (FTCQueue m a x) (FTCQueue m x b)

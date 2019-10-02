{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TATree where

import TSequence

newtype MCont m a b = MCont { appMCont :: a -> m b }

data TATree c a b
	= Leaf (c a b)
	| forall x . Node (TATree c a x) (TATree c x b)

instance TSequence TATree where
	tsingleton = Leaf

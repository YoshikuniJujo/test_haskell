{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.TaggableFunction (TaggableFun) where

import Control.Monad.Freer.Par.Fun

data TaggableFun m a b where
	Open :: Id -> TaggableFun m a a
	Close :: Id -> TaggableFun m a a
	Fun :: (a -> m b) -> TaggableFun m a b

instance Fun TaggableFun where
	fun = Fun
	($$) (Open _) = pure
	($$) (Close _) = pure
	($$) (Fun f) = f

instance Taggable TaggableFun where
	open t = Open t
	close t = Close t
	checkOpen (Open t) (Open t') | t == t' = J t
	checkOpen _ _ = N
	checkClose t (Close t') | t == t' = T
	checkClose _ _ = F

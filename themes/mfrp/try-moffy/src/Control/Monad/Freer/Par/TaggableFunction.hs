{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.TaggableFunction (TaggableFun) where

import Control.Monad.Freer.Par.Fun
import Numeric.Natural

data TaggableFun s m a b where
	Open :: Natural -> TaggableFun s m a a
	Close :: Natural -> TaggableFun s m a a
	Fun :: (a -> m b) -> TaggableFun s m a b

instance Fun (TaggableFun s) where
	fun = Fun
	($$) (Open _) = pure
	($$) (Close _) = pure
	($$) (Fun f) = f

instance Taggable (TaggableFun s) where
	open t = Open t
	close t = Close t
	checkOpen (Open t) (Open t') | t == t' = J (Tag t)
	checkOpen _ _ = N
	checkClose (Tag t) (Close t') | t == t' = T
	checkClose _ _ = F

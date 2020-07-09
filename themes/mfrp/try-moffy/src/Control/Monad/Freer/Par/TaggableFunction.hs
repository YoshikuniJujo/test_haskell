{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.TaggableFunction (TaggableFun) where

import Control.Monad.Freer.Par.Funable (
	Funable(..), Taggable(..), MaybeId(..), Boolean(..), Id )

---------------------------------------------------------------------------

data TaggableFun m a b where
	Open, Close :: Id -> TaggableFun m a a
	Fun :: (a -> m b) -> TaggableFun m a b

instance Funable TaggableFun where
	fun = Fun
	($$) = \case (Open _) -> pure; (Close _) -> pure; (Fun f) -> f

instance Taggable TaggableFun where
	open = Open; close = Close
	checkOpen (Open t) (Open t') | t == t' = J t; checkOpen _ _ = N
	checkClose t (Close t') | t == t' = T; checkClose _ _ = F

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LogicT where

import Control.Monad

class MonadPlus m => MonadLogic m where
	msplit :: m a -> m (Maybe (a, m a))

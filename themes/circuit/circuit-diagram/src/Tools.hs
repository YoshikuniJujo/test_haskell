{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

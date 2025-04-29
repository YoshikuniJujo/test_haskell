{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.StateExcept where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except

stateExceptSample n = do
	State.put (123 :: Int)
	when (n == 0) (Except.throw "zero")
		`Except.catch` \e -> Eff.eff $ putStrLn ("error: " ++ e)
	Eff.eff . print =<< State.get @Int

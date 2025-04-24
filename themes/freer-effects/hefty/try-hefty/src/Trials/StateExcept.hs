{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Trials.StateExcept where

import Control.Monad
import UseFTCQ.Eff qualified as Eff
import UseFTCQ.State qualified as State
import UseFTCQ.Except qualified as Except

stateExceptSample n = do
	State.put (123 :: Int)
	when (n == 0) (Except.throw "zero")
		`Except.catch` \e -> Eff.eff $ putStrLn ("error: " ++ e)
	Eff.eff . print =<< State.get @Int

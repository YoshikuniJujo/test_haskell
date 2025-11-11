{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Trace where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Trace qualified as Trace
import Control.HigherOpenUnion qualified as U

run :: n -> Eff.E '[State.S n, Trace.T] i o r -> IO (r, n)
run n = Trace.run . (`State.run` n)

ignore :: n -> Eff.E '[Trace.T, State.S n] i o r -> (r, n)
ignore n = Eff.run . (`State.run` n) . Trace.ignore

calc :: (U.Member (State.S Int) es, U.Member Trace.T es) =>
	Eff.E es i o ()
calc = do
	Trace.trace "add 2"
	State.modify @Int (+ 2)
	Trace.trace "multiply by 5"
	State.modify @Int (* 5)
	Trace.trace "subtract 4"
	State.modify @Int (subtract 4)

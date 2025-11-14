{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.Trace where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Trace qualified as Trace
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

runCalc :: IO ((), Int)
runCalc = run 6 calc

ignoreCalc :: ((), Int)
ignoreCalc = ignore 6 calc

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

runIOSample :: IO ()
runIOSample = runIO sample

ignoreIOSample :: IO ()
ignoreIOSample = ignoreIO sample

runIO :: Eff.E '[Trace.T, IO.I] i o r -> IO r
runIO = Eff.runM . Trace.runIO

ignoreIO :: Eff.E '[Trace.T, IO.I] i o r -> IO r
ignoreIO = Eff.runM . Trace.ignore

sample :: (U.Member Trace.T es, U.Base IO.I es) => Eff.E es i o ()
sample = do
	Trace.trace "I'll say hello now"
	IO.putStrLn "Hello, world!"
	Trace.trace "I said hello"

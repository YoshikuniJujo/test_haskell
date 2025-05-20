{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.State where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.StateNew qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

sampleStateNew :: (
	U.Member (State.S Int) es,
	U.Member (Except.E String) es,
	U.Base IO.I es ) =>
	Eff.E es i o ()
sampleStateNew = do
	State.put @Int 111
	sampleStateNewInner `Except.catch` IO.print @String
	IO.print =<< State.get @Int

sampleStateNew' :: (
	U.Member (State.S Int) es,
	U.Member (Except.E String) es,
	U.Base IO.I es ) =>
	Eff.E es i o ()
sampleStateNew' = do
	State.put @Int 111
	State.transaction Int sampleStateNewInner `Except.catch` IO.print @String
	IO.print =<< State.get @Int

sampleStateNewInner :: (
	U.Member (State.S Int) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o ()
sampleStateNewInner = do
	State.put @Int 123
	_ <- Except.throw "bad"
	State.put @Int 789

runSampleStateNew1 ::
	Eff.E '[
		Except.E String, State.S Int, IO.I 
		] i o a -> IO (Either String a, Int)
runSampleStateNew1 = Eff.runM . (`State.run` 0) . Except.run

runSampleStateNew2 ::
	Eff.E '[
		State.S Int, Except.E String, IO.I 
		] i o a -> IO (Either String (a, Int))
runSampleStateNew2 = Eff.runM . Except.run . (`State.run` 0)

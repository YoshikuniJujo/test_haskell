{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryYaftee.State where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Reader qualified as Reader
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

run :: d -> a -> Eff.E '[Reader.R d, State.S a] i o r -> (r, a)
run d x0 = Eff.run . (`State.run` x0) . (`Reader.run` d)

increase ::
	(U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Eff.E es i o ()
increase = do
	d <- Reader.ask @Int
	State.modify (+ d)

increaseNTimes :: (U.Member (Reader.R Int) es, U.Member (State.S Int) es) =>
	Int -> Eff.E es i o ()
increaseNTimes n = replicateM_ n increase

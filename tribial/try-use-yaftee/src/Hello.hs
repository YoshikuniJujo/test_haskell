{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hello where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

tryM :: Monad m => Eff.E '[State.S Int, U.FromFirst m] i o a -> m (a, Int)
tryM = Eff.runM . (`State.run` 0)

hello :: U.Base IO.I es => Eff.E es i o ()
hello = IO.putStrLn "Hello"

count :: U.Member (State.S Int) es => Eff.E es i o ()
count = State.modify (+ (1 :: Int))

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Except where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.Exception

tryExceptIO :: forall e i o a . Exception e => Eff.E '[Except.E e, IO.I] i o a -> IO a
tryExceptIO = Eff.runM . Except.runIO @e

error' :: String -> Eff.E es i o r
error' = throw . ErrorCall

tryExceptState :: Eff.E '[Except.E e, State.S Int] i o r -> (Either e r, Int)
tryExceptState = Eff.run . (`State.run` 0) . Except.run

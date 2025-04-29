{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Gzip where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as Union
import Data.ByteString qualified as BS

import Pipe.ByteString.OnDemand

readMagic :: (
	Union.Member (State.S Request) effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) BS.ByteString
readMagic = do
	State.put $ RequestBytes 2
	Pipe.await

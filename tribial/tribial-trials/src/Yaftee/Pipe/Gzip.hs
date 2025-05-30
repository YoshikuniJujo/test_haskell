{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.Pipe.Gzip where

import Data.ByteString qualified as BS
import Data.BitArray qualified as BitArray

import Yaftee.Eff qualified as Eff
import Yaftee.Pipe qualified as Pipe
import Yaftee.State qualified as State
import Yaftee.OpenUnion qualified as Union

import Yaftee.Pipe.ByteString.OnDemand

readMagic :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs ) =>
	Eff.E effs BS.ByteString o BS.ByteString
readMagic = do
	State.put $ RequestBytes 2
	Pipe.await

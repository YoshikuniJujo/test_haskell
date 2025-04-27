{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.Legacy.Pipe.Gzip where

import Data.ByteString qualified as BS
import Data.BitArray qualified as BitArray

import Yaftee.Eff qualified as Eff
import Yaftee.Legacy.NewPipe qualified as Pipe
import Yaftee.State qualified as State
import Yaftee.OpenUnion qualified as Union

import Yaftee.Legacy.Pipe.ByteString.OnDemand

readMagic :: (
	Union.Member Pipe.Await effs,
	Union.Member (State.S Request) effs
	) =>
	Eff.E effs (Either BitArray.B BS.ByteString) o
		(Either BitArray.B BS.ByteString)
readMagic = do
	State.put $ RequestBytes 2
	Pipe.await

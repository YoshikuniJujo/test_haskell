{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Compress (compress) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.GzipHeader
import Data.Gzip.Block

compress :: (
	U.Member Pipe.P es,
	U.Member (State.Named "foobar" PipeCrc.Crc32) es,
	U.Member (State.Named "foobar" PipeBS.Length) es,
	RunLength.Members "foobar" es,
	U.Member (State.Named "foobar" PipeB.Queue) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
compress = void $
	PipeCrc.crc32' "foobar" Pipe.=$= PipeBS.length' "foobar" Pipe.=$=
	RunLength.compress "foobar" Pipe.=$= PipeL.bundle' 500 Pipe.=$=
	PipeT.convert'' runLengthsToBits [] Pipe.=$= do
		Pipe.yield $ encodeGzipHeader sampleGzipHeader
		PipeB.toByteString' "foobar"
		PipeCrc.compCrc32 "foobar"
		Pipe.yield . PipeCrc.crc32ToByteString =<< State.getN "foobar"
		Pipe.yield . PipeBS.lengthToByteString =<< State.getN "foobar"

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Data.ByteString qualified as BS
import Data.ByteString.Bit qualified as Bit
import System.IO
import System.Environment

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.GzipHeader
import Data.Gzip.Block

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	void $ withFile ifp ReadMode \r -> withFile ofp WriteMode \o -> Eff.runM
		. (`State.run` Bit.empty)
		. (`State.run` ("" :: BS.ByteString))
		. PipeBS.lengthRun @"foobar" -- (`State.run` FileLength 0)
		. PipeCrc.runCrc32 @"foobar" . RunLength.run . PipeL.to
		$ PipeBS.hGet 64 r Pipe.=$=
			PipeCrc.crc32' "foobar" Pipe.=$=
			PipeBS.length' "foobar" Pipe.=$=
			RunLength.compressRL Pipe.=$=
			PipeL.bundle' 500 Pipe.=$=
			PipeT.convert'' runLengthsToBits [] Pipe.=$= do
				Pipe.yield $ encodeGzipHeader sampleGzipHeader
				PipeB.toByteString'
				PipeCrc.compCrc32 "foobar"
				Pipe.yield . PipeCrc.crc32ToByteString =<< State.getN "foobar"
				Pipe.yield . PipeBS.lengthToByteString =<< State.getN "foobar"
			Pipe.=$= PipeBS.hPutStr' o

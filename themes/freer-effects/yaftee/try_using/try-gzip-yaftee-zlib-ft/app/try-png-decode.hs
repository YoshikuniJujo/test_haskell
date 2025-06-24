{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ

import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- CByteArray.malloc 64
	ob <- CByteArray.malloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. PipeZ.inflateRun @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$= do
				fhdr <- Chunk.readBytes "foobar" 8
				IO.print fhdr
				Chunk.chunk "foobar" 500
			Pipe.=$= do
				OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" IO.print
				PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ib ob
			Pipe.=$= PipeIO.print
	CByteArray.free ib
	CByteArray.free ob

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
-- import Data.Gzip.Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @Zlib.ReturnCode
		. PipeZ.run @"foobar"
		. Pipe.run
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$= do
--				Pipe.yield . BSF.fromStrict
--					$ encodeGzipHeader sampleGzipHeader
				PipeZ.deflate "foobar" IO sampleOptions ib ob
--				forever $ Pipe.yield =<< Pipe.await
			Pipe.=$=
				PipeIO.print

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsRaw 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 8,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

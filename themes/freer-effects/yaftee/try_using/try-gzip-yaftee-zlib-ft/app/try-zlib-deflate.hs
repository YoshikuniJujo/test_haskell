{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment

import Codec.Compression.Zlib.Structure.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @Zlib.ReturnCode
		. PipeZ.run @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			PipeZ.deflate "foobar" IO sampleOptions ib ob Pipe.=$=
			PipeT.convert BSF.toStrict Pipe.=$=
			PipeBS.hPutStr ho
	PipeZ.cByteArrayFree ib; PipeZ.cByteArrayFree ob
	hClose h; hClose ho

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

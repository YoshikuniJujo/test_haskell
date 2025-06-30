{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.MonoTraversable qualified as PipeMT
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.Word
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import Data.Gzip.Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Word.Crc32 qualified as Crc32

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @Zlib.ReturnCode
		. PipeZ.run @"foobar"
		. PipeCrc32.run @"foobar"
		. PipeMT.lengthRun @"foobar"
		. Pipe.run
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h Pipe.=$= do
				PipeT.convert' BSF.fromStrict
				IO.putStrLn "INPUT END"
			Pipe.=$= do
				Pipe.yield . BSF.fromStrict
					$ encodeGzipHeader sampleGzipHeader
				PipeMT.length' "foobar" Pipe.=$= PipeCrc32.crc32 "foobar" Pipe.=$=
					PipeZ.deflate "foobar" IO sampleOptions ib ob
				PipeCrc32.complement "foobar"
				crc <- BSF.fromStrict . BS.fromBits' . Crc32.toWord
					<$> State.getN @Crc32.C "foobar"
--				IO.print crc
				Pipe.yield crc
				ln <- BSF.fromStrict . BS.fromBits' . fromIntegral @_ @Word32 . PipeMT.lengthToInt64
					<$> State.getN @PipeMT.Length "foobar"
--				IO.print ln
				Pipe.yield ln
--				forever $ Pipe.yield =<< Pipe.await
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeIO.debugPrint
			Pipe.=$= do
				PipeBS.hPutStr ho
--				PipeIO.print'
	hClose ho

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsRaw 15,
--	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 8,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

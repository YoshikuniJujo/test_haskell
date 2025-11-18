{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pp
import Control.Monad.Yaftee.Pipe.Tools qualified as PpT
import Control.Monad.Yaftee.Pipe.IO qualified as PpIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PpBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PpZ
import Control.Monad.Yaftee.Except qualified as Ex
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment
import Codec.Compression.Zlib.Constant.Core qualified as Z
import Codec.Compression.Zlib.Advanced.Core qualified as Z

main :: IO ()
main = do
	fp : as <- getArgs; h <- openFile fp ReadMode
	let	opt = case as of
			[] -> options $ Z.WindowBitsRaw 15
			["zlib"] -> options $ Z.WindowBitsZlib 15
			["gzip"] -> options $ Z.WindowBitsGzip 15
			_ -> error "bad arguments"
	ib <- PpZ.cByteArrayMalloc 64; ob <- PpZ.cByteArrayMalloc 64
	_ <- Eff.runM . Ex.run @Z.ReturnCode . PpZ.run @"" . Pp.run
		. (`Ex.catch` IO.print @Z.ReturnCode) . void $ PpBS.hGet 32 h
			Pp.=$= PpT.convert BSF.fromStrict
			Pp.=$= PpZ.deflate "" IO opt ib ob
			Pp.=$= PpIO.print
	PpZ.cByteArrayFree ib; PpZ.cByteArrayFree ob; hClose h

options :: Z.WindowBits -> PpZ.DeflateOptions
options wb = PpZ.DeflateOptions {
	PpZ.deflateOptionsCompressionLevel = Z.DefaultCompression,
	PpZ.deflateOptionsCompressionMethod = Z.Deflated,
	PpZ.deflateOptionsWindowBits = wb,
	PpZ.deflateOptionsMemLevel = Z.MemLevel 8,
	PpZ.deflateOptionsCompressionStrategy = Z.DefaultStrategy }

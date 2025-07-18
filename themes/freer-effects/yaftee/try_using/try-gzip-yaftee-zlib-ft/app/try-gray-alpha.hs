{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode

main :: IO ()
main = do
	fp : fpo : _ <- getArgs

	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	print hdr

	let	rs = (+ 1) <$> Header.headerToRows hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64

	ho <- openFile fpo WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"foobar" @BSF.ByteString
		. Buffer.run @"barbaz" @BSF.ByteString
		. PipeZ.run @"foobar"
		. PipeZ.run @"barbaz"
		. Steps.chunkRun_ @"foobar"
		. Pipe.run
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Steps.chunk "foobar"
			Pipe.=$= forever do
				bs <- Pipe.await
				cnk <- State.getN @Steps.Chunk "foobar"
				when (cnk == Steps.Chunk "IDAT") $ Pipe.yield bs
			Pipe.=$= PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ibd obd
			Pipe.=$= Buffer.format "foobar" BSF.splitAt' "" rs
			Pipe.=$= Unfilter.pngUnfilter' hdr
			Pipe.=$= PipeT.convert (wordsToGrayAlphas hdr)

--			Pipe.=$= PipeIO.debugPrint

			Pipe.=$= Encode.encodeGrayAlpha "barbaz" IO hdr ibe obe
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho

	hClose h; hClose ho

wordsToGrayAlphas :: RealFrac d => Header.Header -> [Word8] -> [GrayAlpha d]
wordsToGrayAlphas = \case
	Header.Header { Header.headerBitDepth = 8 } -> wordsToGrayAlphas8
	Header.Header { Header.headerBitDepth = 16 } -> wordsToGrayAlpha16

wordsToGrayAlphas8 :: RealFrac d => [Word8] -> [GrayAlpha d]
wordsToGrayAlphas8 [] = []
wordsToGrayAlphas8 (x : a : ws) = GrayAlphaWord8 x a : wordsToGrayAlphas8 ws

wordsToGrayAlpha16 :: RealFrac d => [Word8] -> [GrayAlpha d]
wordsToGrayAlpha16 [] = []
wordsToGrayAlpha16 (
	(fromIntegral -> x1) : (fromIntegral -> x0) :
	(fromIntegral -> a1) : (fromIntegral -> a0) : ws ) =
	GrayAlphaWord16 (x1 `shiftL` 8 .|. x0) (a1 `shiftL` 8 .|. a0) :
	wordsToGrayAlpha16 ws

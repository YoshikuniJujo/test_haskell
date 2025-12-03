{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Header qualified as Png
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Lifegame.Words qualified as Lifegame

import PngToImageGray1
import Data.Word.Crc32 qualified as Crc32
import Control.Monad.Yaftee.Pipe.Png.ChunkDecode qualified as Chunk

import System.File.Png.Gray1.NoInterlace qualified as Png

main :: IO ()
main = do
	fp : sz : fpo : _ <- getArgs
	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String . Png.runHeader' @"barbaz"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Crc32.initial
		. Fail.run
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= Png.decodeHeader' "foobar" "barbaz"
	hClose hh
	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode . Fail.run
		. runPngToImageGray1 @"foobar" @BSF.ByteString
		. Chunk.run_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h
		Pipe.=$= pngToImageGray1 "foobar" hdr ibd obd
		Pipe.=$= (Pipe.await >>=) \img -> Eff.effBase do
			let	brd = Lifegame.gray1ToBoard img
			Png.write fpo (Lifegame.boardToGray1' (read sz) brd)
	PipeZ.cByteArrayFree ibd
	PipeZ.cByteArrayFree obd
	hClose h

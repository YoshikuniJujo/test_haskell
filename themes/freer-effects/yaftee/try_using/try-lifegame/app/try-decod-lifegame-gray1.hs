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
import Control.Monad.Yaftee.Pipe.Png.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Lifegame.Words qualified as Lifegame

import PngToImageGray1
import Lifegame.Png.Chunk.Decode qualified as Chunk

import System.File.Png.Gray1.NoInterlace qualified as Png

main :: IO ()
main = do
	fp : sz : fpo : _ <- getArgs
	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String . Header.run @"foobar" @"barbaz"
		. Fail.run
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= Header.decode "foobar" "barbaz"
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

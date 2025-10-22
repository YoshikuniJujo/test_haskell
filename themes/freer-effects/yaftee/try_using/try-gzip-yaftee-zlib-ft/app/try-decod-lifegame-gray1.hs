{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import Data.Image.Gray1 qualified as ImageG1
import System.IO
import System.Environment
import System.File.Png.Gray1.NoInterlace qualified as Png
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Lifegame.Words qualified as Lifegame

import PngToImageGray1

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String . Png.runHeader @"foobar"
		. Pipe.run . (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= Png.decodeHeader "foobar"
	hClose hh
	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"foobar" @BSF.ByteString . PipeZ.run @"foobar"
		. Steps.chunkRun_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h
		Pipe.=$= pngToImageGray1 "foobar" hdr ibd obd
		Pipe.=$= (Pipe.await >>=) \img -> Eff.effBase do
			let	brd = Lifegame.gray1ToBoard img
				img' n = Lifegame.boardToGray1' n brd
			ImageG1.printAsAscii $ img' 3
			Png.write fpo $ img' 15
	PipeZ.cByteArrayFree ibd
	PipeZ.cByteArrayFree obd
	hClose h

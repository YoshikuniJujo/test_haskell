{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Png.Lifegame (
	readBoard, writeBoard
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO

import Data.ByteString.FingerTree qualified as BSF

import System.IO
import System.File.Png.Gray1.NoInterlace qualified as Png
import Lifegame.Words qualified as Lifegame

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps

import PngToImageGray1

writeBoard :: FilePath -> Lifegame.Board -> Int -> IO ()
writeBoard fpo bd n = Png.write fpo (Lifegame.boardToGray1' n bd)

readBoard :: FilePath -> IO Lifegame.Board
readBoard fp = do
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
	(_x, y) <- Eff.runM
		. (`State.run` Lifegame.emptyBoard 0 0)
		. Except.run @String . Except.run @Zlib.ReturnCode
		. runPngToImageGray1 @"foobar" @BSF.ByteString
		. Steps.chunkRun_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h
		Pipe.=$= pngToImageGray1 "foobar" hdr ibd obd
		Pipe.=$= (Pipe.await >>=) \img -> do
			let	brd = Lifegame.gray1ToBoard img
			State.put brd
	PipeZ.cByteArrayFree ibd
	PipeZ.cByteArrayFree obd
	hClose h
	pure y

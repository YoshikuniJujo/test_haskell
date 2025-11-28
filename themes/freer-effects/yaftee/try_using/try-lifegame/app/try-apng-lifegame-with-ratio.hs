{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.Foldable
import Data.List qualified as L
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header.Data qualified as Header
import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import PngToImageGray1

import System.File.Apng.Gray1.NoInterlace
import Lifegame.Words qualified as Lifegame

import Data.Word.Crc32 qualified as Crc32

main :: IO ()
main = do
	dr : d_ : de_ : fpo : _ <- getArgs

	fps@(fp0 : _) <- ((dr </>) <$>)
		. L.sort . filter (not . ("." `L.isSuffixOf`)) <$> getDirectoryContents dr

	hh <- openFile fp0 ReadMode

	Right hdr <- Eff.runM . Except.run @String . Png.runHeader @"foobar"
		. flip (State.runN @"foobar") Crc32.initial
		. Fail.run
		. Pipe.run . (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	let	n = length fps
		wdt = Header.headerWidth hdr
		hgt = Header.headerHeight hdr

	print fps
	print (wdt, hgt)

	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM
		. Steps.chunkRun_ @"foobar"
		. runPngToImageGray1 @"foobar" @BSF.ByteString
		. Except.run @String . Except.run @Zlib.ReturnCode . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ for_ fps (\fp ->
			do
				h <- Eff.effBase $ openFile fp ReadMode
				PipeBS.hGet 32 h
				Eff.effBase $ hClose h
			Pipe.=$= do
				pngToImageGray1 "foobar" hdr ibd obd
				Bytes.flush "foobar")

		Pipe.=$= (Pipe.yield =<< replicateM n Pipe.await)
		Pipe.=$= PipeT.convert ((Lifegame.boardToGray1' 10 . Lifegame.gray1ToBoard) <$>)
		Pipe.=$= PipeT.convert ((, read d_) <$>)
		Pipe.=$= do
			fs <- Pipe.await
			Eff.effBase $ writeApngGray1Foo' fpo hdr n 0 fs
	putStrLn ""
	print hdr
	print Header.Header {
		Header.headerWidth = 20, Header.headerHeight = 11,
		Header.headerBitDepth = 1,
		Header.headerColorType = Header.ColorTypeGrayscale,
		Header.headerCompressionMethod = Header.CompressionMethodDeflate,
		Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
		Header.headerInterlaceMethod = Header.InterlaceMethodNon }

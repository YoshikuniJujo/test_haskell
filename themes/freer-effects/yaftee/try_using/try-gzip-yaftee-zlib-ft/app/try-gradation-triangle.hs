{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header
import System.IO
import System.Environment
import System.FilePath

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Simple qualified as Image

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode

import "try-gzip-yaftee-zlib-ft" Tools

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String . Png.runHeader @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict Pipe.=$= do
				Png.decodeHeader "foobar"
	hClose hh

	print hdr
	img <- Image.new
		(fromIntegral $ Header.headerWidth hdr)
		(fromIntegral $ Header.headerHeight hdr)
	print $ Header.headerColorType hdr
	print $ Header.headerBitDepth hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	ho <- openFile fpo WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString
		. Buffer.run @"foobar" @BSF.ByteString
		. PipeZ.run @"foobar"
		. PipeZ.run @"barbaz"
		. Fail.runExc id id
		. Steps.chunkRun_ @"foobar"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h

			Pipe.=$= PipeT.convert BSF.fromStrict Pipe.=$=
				Steps.chunk "foobar"
			Pipe.=$= (fix \go -> Pipe.awaitMaybe >>= \case
				Nothing -> pure ()
				Just bd -> do
					bd' <- if BSF.null bd then Pipe.await else pure bd
					Steps.Chunk { Steps.chunkName = nm } <-
						State.getN @Steps.Chunk "foobar"
					if nm == "IHDR"
					then void $ Pipe.yield bd'
						Pipe.=$= OnDemand.onDemand "foobar"
						Pipe.=$= Header.read "foobar" (const $ pure ())
					else when (nm == "IDAT") $ Pipe.yield bd'
					void go)
			Pipe.=$= PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ibd obd
			Pipe.=$= do
				bs0 <- Pipe.await
				rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN "foobar"
				Buffer.format "foobar" BSF.splitAt' bs0 rs
			Pipe.=$= Unfilter.pngUnfilter "foobar"
			Pipe.=$= PipeT.convert (Header.word8ListToRgbaList @Double hdr)

			Pipe.=$= pipeZip (Header.headerToPoss hdr)
			Pipe.=$= forever do
				clrs <- Pipe.await
				(\(clr, (x, y)) -> Eff.effBase $ Image.write @IO img x y clr) `mapM_` clrs
				Pipe.yield (fst <$> clrs)

			Pipe.=$= Encode.encodeRgba "barbaz" IO hdr ibe obe

			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho
	hClose h; hClose ho

	let	(fpbd, fpex) = splitExtension fpo
		fpo' = fpbd ++ "-img" <.> fpex
	ho' <- openFile fpo' WriteMode

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString
		. PipeZ.run @"barbaz"
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ fromImage @Double IO img (Header.headerToPoss' hdr)
			Pipe.=$= Encode.encodeRgba "barbaz" IO hdr ibe obe
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho'

	hClose ho'

data Chunk = Chunk {
	chunkName :: BSF.ByteString,
	chunkBody :: BSF.ByteString }
	deriving Show

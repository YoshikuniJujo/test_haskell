{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png qualified as Png
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	ho <- openFile fpo WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. PipeZ.run @"foobar"
		. PipeZ.run @"barbaz"
		. Fail.runExc id
		. Steps.chunkRun_ @"foobar"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. void $ PipeBS.hGet 32 h
--			Pipe.=$= PipeIO.debugPrint
			Pipe.=$= PipeT.convert BSF.fromStrict Pipe.=$=
				Steps.chunk "foobar"
			Pipe.=$= (fix \go -> Pipe.awaitMaybe >>= \case
				Nothing -> pure ()
				Just bd -> do
					bd' <- if BSF.null bd then Pipe.await else pure bd
					Steps.Chunk nm <-
						State.getN @Steps.Chunk "foobar"
					if nm == "IHDR"
					then void $ Pipe.yield bd'
						Pipe.=$= OnDemand.onDemand "foobar"
						Pipe.=$= Header.read "foobar" (const $ pure ())
					else when (nm == "IDAT") $ Pipe.yield bd'
					void go)
			Pipe.=$= PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ibd obd
			Pipe.=$= PipeZ.deflate "barbaz" IO sampleOptions ibe obe
			Pipe.=$= do
				bd <- Pipe.await
				bd'' <- Header.encodeHeader <$> State.getN "foobar"
				Pipe.yield $ Chunk {
					chunkName = "IHDR",
					chunkBody = BSF.fromStrict bd'' }
				Pipe.yield $ Chunk {
					chunkName = "IDAT",
					chunkBody = bd }
				fix \go -> Pipe.awaitMaybe >>= \case
					Nothing -> pure ()
					Just bd -> do
						Pipe.yield Chunk {
							chunkName = "IDAT",
							chunkBody = bd }
						go
				Pipe.yield Chunk {
					chunkName = "IEND", chunkBody = "" }
			Pipe.=$= do
				Pipe.yield Png.fileHeader
				PipeT.convert chunkToByteString
			Pipe.=$= PipeT.convert BSF.toStrict
--			Pipe.=$= PipeIO.debugPrint
			Pipe.=$= PipeBS.hPutStr ho
	hClose h; hClose ho

data Chunk = Chunk {
	chunkName :: BSF.ByteString,
	chunkBody :: BSF.ByteString }
	deriving Show

chunkToByteString :: Chunk -> BSF.ByteString
chunkToByteString Chunk { chunkName = nm, chunkBody = bd } =
	BSF.fromBitsBE' ln <> nmbd <> BSF.fromBitsBE' (Crc32.toWord crc)
	where
	ln = fromIntegral @_ @Word32 $ BSF.length bd
	nmbd = nm <> bd
	crc = Crc32.complement $ BSF.foldl' Crc32.step Crc32.initial nmbd

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

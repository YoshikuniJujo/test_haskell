{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Image.Simple qualified as Image
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header

import System.IO
import System.Environment
import System.FilePath

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Apng.Decode

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer

import Data.IORef

import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree.Bits qualified as BSF
import Control.Monad.Yaftee.Pipe.Png.Palette qualified as Palette
import Data.Png qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Apng

main :: IO ()
main = do
	fp : fpo : _ <- getArgs

	imgs <- newIORef []
	fctls <- newIORef []
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64

	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	print hdr

	print =<< length <$> readIORef imgs

	let	fn = 208

	imgs_ <- replicateM fn $ Image.new @IO 100 100

	writeIORef imgs imgs_

	for_ [0 .. fn - 1] \n -> do
		print n
		img <- (!! n) <$> readIORef imgs
		(\y -> (\x -> Image.write @IO img x y $ gradation n)
				`mapM_` [0 .. 99]) `mapM_` [0 .. 99]

	writeIORef fctls . sampleFctls $ fromIntegral fn

	writePng (filePath fpo 0) hdr fctls imgs ibe obe

gradation :: Int -> Rgba Double
gradation n
	| 0 <= n && n < 52 = RgbaWord8 @Double
		(5 * fromIntegral n)
		0
		(255 - 5 * fromIntegral n)
		255
	| 52 <= n && n < 104 = let n' = n - 52 in RgbaWord8 @Double
		255
		(5 * fromIntegral n')
		0
		255
	| 104 <= n && n < 156 = let n' = n - 104 in RgbaWord8 @Double
		(255 - 5 * fromIntegral n')
		255
		0
		255
	| 156 <= n && n < 208 = let n' = n - 156 in RgbaWord8 @Double
		0
		(255 - 5 * fromIntegral n')
		(5 * fromIntegral n')
		255

sampleFctls :: Word32 -> [Fctl]
sampleFctls fn = mkFctl 0 : (mkFctl <$> [1, 3 .. fn * 2 - 3])

mkFctl :: Word32 -> Fctl
mkFctl sn = Fctl {
	fctlSequenceNumber = sn,
	fctlWidth = 100,
	fctlHeight = 100,
	fctlXOffset = 0,
	fctlYOffset = 0,
	fctlDelayNum = 75,
	fctlDelayDen = 1000,
	fctlDisposeOp = 1,
	fctlBlendOp = 0 }

writePng ::
	FilePath -> Header.Header ->
	IORef [Fctl] -> IORef [Image.I RealWorld] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> IO ()
writePng fpp hdr fctls_ imgs_ ibe obe = do
	ho <- openFile fpp WriteMode
	fctls <- readIORef fctls_
	imgs <- readIORef imgs_

	hWritePng ho hdr fctls imgs ibe obe

	hClose ho

hWritePng :: (Monad m, U.Base (U.FromFirst IO) '[U.FromFirst m]) =>
	Handle -> Header.Header -> [Fctl] -> [Image.I RealWorld] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> m ()
hWritePng ho hdr fctls imgs ibe obe =
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.run
		. Buffer.run @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipe ho hdr fctls imgs ibe obe

hWritePngPipe :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> [Fctl] -> [Image.I RealWorld] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipe ho hdr fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromFctlImages IO hdr fctls imgs
		Pipe.=$= encodeApng hdr fctls ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

fromFctlImages :: forall m -> (
	PrimMonad m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header ->
	[Fctl] ->
	[Image.I (PrimState m)] ->
	Eff.E es i Body ()
fromFctlImages _ _ [] [] = pure ()
fromFctlImages m hdr (fctl : fctls) (img : imgs) = do
	Pipe.yield $ BodyFctl fctl
	fromImage' m fctl img (fctlPoss' hdr fctl)
	fromFctlImages m hdr fctls imgs
fromFctlImages _ _ _ _ = error "bad"

fromImage' :: forall m -> (
	PrimMonad m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst m) es
	) =>
	Fctl ->
	Image.I (PrimState m) -> [[(Int, Int)]] ->
	Eff.E es i Body ()
fromImage' m fctl img = \case
	[] -> pure ()
	ps : pss -> do
		Pipe.yield . BodyRgba =<< (\(x, y) -> Eff.effBase $ Image.read @m img x y) `mapM` ps
		fromImage' m fctl img pss

encodeApng :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base (U.FromFirst IO) es ) =>
	Header.Header -> [Fctl] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es Body BSF.ByteString ()
encodeApng _ [] _ _ = pure ()
encodeApng hdr fctls@(fctl : _) ibe obe =
	encodeRawCalc "barbaz" IO hdr {
		Header.headerWidth = fctlWidth fctl,
		Header.headerHeight = fctlHeight fctl }
		(fctlToSize <$> fctls)
		Nothing ibe obe

fctlToSize :: Fctl -> (Word32, Word32)
fctlToSize c = (fctlWidth c, fctlHeight c)

filePath :: FilePath -> Int -> FilePath
filePath fpo n = fpbd ++ "-" ++ showN 2 n <.> fpex
	where (fpbd, fpex) = splitExtension fpo

showN :: Show n => Int -> n -> String
showN ln n = replicate (ln - length s) '0' ++ s where s = show n

encodeRawCalc :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Member U.Fail es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header ->
	[(Word32, Word32)] -> Maybe Palette.Palette ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es Body BSF.ByteString ()
encodeRawCalc _ _ _ [] _ _ _ = pure ()
encodeRawCalc nm m hdr ((w0, h0) : sz) mplt ibe obe = void $
-- MAKE IDAT
	do	BodyFctl fctl0 <- Pipe.await
		Pipe.yield . Chunk "fcTL" $ encodeFctl fctl0
		pipeDat nm m False 0 hdr w0 h0 ibe obe
		for_ sz \(w, h) -> do
			BodyFctl fctl <- Pipe.await
			Pipe.yield . Chunk "fcTL" $ encodeFctl fctl
			pipeDat nm m True (fctlSequenceNumber fctl + 1) hdr w h ibe obe

-- MAKE CHUNKS

	Pipe.=$= do
		let	bd'' = Header.encodeHeader hdr
		Pipe.yield $ Chunk {
			chunkName = "IHDR",
			chunkBody = BSF.fromStrict bd'' }

		case mplt of
			Nothing -> pure ()
			Just plt -> Pipe.yield $ Chunk {
				chunkName = "PLTE",
				chunkBody = Palette.encodePalette plt }

		Pipe.yield $ Chunk {
			chunkName = "acTL",
			chunkBody = encodeActl $ Actl 208 0 }

		bd0 <- Pipe.await
		Pipe.yield bd0
		fix \go -> Pipe.awaitMaybe >>= \case
			Nothing -> pure ()
			Just bd -> (>> go) $ Pipe.yield bd

		Pipe.yield Chunk {
			chunkName = "IEND", chunkBody = "" }
	Pipe.=$= do
		Pipe.yield Png.fileHeader
		PipeT.convert chunkToByteString

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

pipeDat :: forall nm m -> (
	Encode.Datable a,
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Bool -> Word32 ->
	Header.Header -> Word32 -> Word32 ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es a Chunk ()
pipeDat nm m iorf sn hdr w h ibe obe = void $
	(fix \go -> do
		x <- Pipe.await
		if Encode.endDat x then pure () else Pipe.yield x >> go)
	Pipe.=$= PipeT.convert (Encode.toDat hdr)
	Pipe.=$= do
		bs0 <- Pipe.await
		Unfilter.pngFilter hdr bs0 $ Header.calcSizes hdr w h
	Pipe.=$= PipeT.convert BSF.pack
	Pipe.=$= PipeZ.deflate nm m sampleOptions ibe obe
--	Pipe.=$= Buffer.format' nm BSF.splitAt' "" 5000
	Pipe.=$= Buffer.format' nm BSF.splitAt' "" 100000
	Pipe.=$= PipeT.convert (bool (Chunk "IDAT") (Chunk "fdAT" . (BSF.fromBitsBE' sn <>)) iorf)

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

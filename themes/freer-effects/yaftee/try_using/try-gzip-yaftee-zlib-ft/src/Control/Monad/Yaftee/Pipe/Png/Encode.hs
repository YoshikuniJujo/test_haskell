{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Encode (

	encodeRgba, encodeGray8, encodeGrayAlpha,

	encodeRaw

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Palette
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.MonoTraversable
import Data.Vector qualified as V
import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Color
import Data.Png qualified as Png
import Data.Png.Header qualified as Header

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

encodeRgba :: forall nm m -> (
	PrimBase m, RealFrac d, U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es ) =>
	Header.Header ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es [Rgba d] BSF.ByteString ()
encodeRgba nm m hdr ibe obe =
	void $ PipeT.convert (Header.rgbaListToWord8List hdr)
		Pipe.=$= PipeT.convert BSF.pack

		Pipe.=$= encodeRaw nm m hdr Nothing ibe obe

encodeGray8 :: forall nm m -> (
	PrimBase m, RealFrac d, U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es ) =>
	Header.Header ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es [Gray d] BSF.ByteString ()
encodeGray8 nm m hdr ibe obe =
	void $ PipeT.convert (graysToWords hdr)
		Pipe.=$= PipeT.convert BSF.pack
		Pipe.=$= encodeRaw nm m hdr Nothing ibe obe

encodeGrayAlpha :: forall nm m -> (
	PrimBase m, RealFrac d, U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es ) =>
	Header.Header ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es [GrayAlpha d] BSF.ByteString ()
encodeGrayAlpha nm m hdr ibe obe =
	void $ PipeT.convert (grayAlphaToWords hdr)
		Pipe.=$= PipeT.convert BSF.pack
		Pipe.=$= encodeRaw nm m hdr Nothing ibe obe

graysToWords :: RealFrac d => Header.Header -> [Gray d] -> [Word8]
graysToWords = \case
	Header.Header { Header.headerBitDepth = 1 } -> graysToWords1
	Header.Header { Header.headerBitDepth = 2 } -> graysToWords2
	Header.Header { Header.headerBitDepth = 4 } -> graysToWords4
	Header.Header { Header.headerBitDepth = 8 } ->
		((\(GrayWord8 w) -> w) <$>)
	Header.Header { Header.headerBitDepth = 16 } -> concat
		. ((\(GrayWord16 w) ->
			[fromIntegral (w `shiftR` 8), fromIntegral w]) <$>)
	_ -> error "yet"

grayAlphaToWords :: RealFrac d => Header.Header -> [GrayAlpha d] -> [Word8]
grayAlphaToWords = \case
	Header.Header { Header.headerBitDepth = 8 } -> grayAlphasToWords8
	Header.Header { Header.headerBitDepth = 16 } -> grayAlphasToWords16

graysToWords1 :: RealFrac d => [Gray d] -> [Word8]
graysToWords1 [] = []
graysToWords1 [GrayWord1 x] = [x `shiftL` 7]
graysToWords1 [GrayWord1 x, GrayWord1 y] = [x `shiftL` 7 .|. y `shiftL` 6]
graysToWords1 [GrayWord1 x, GrayWord1 y, GrayWord1 z] =
	[x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5]
graysToWords1 [GrayWord1 x, GrayWord1 y, GrayWord1 z, GrayWord1 w] =
	[x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5 .|. w `shiftL` 4]
graysToWords1 [
	GrayWord1 x, GrayWord1 y, GrayWord1 z, GrayWord1 w, GrayWord1 v ] = [
	x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5 .|. w `shiftL` 4 .|.
	v `shiftL` 3 ]
graysToWords1 [
	GrayWord1 x, GrayWord1 y, GrayWord1 z, GrayWord1 w,
	GrayWord1 v, GrayWord1 u ] = [
	x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5 .|. w `shiftL` 4 .|.
	v `shiftL` 3 .|. u `shiftL` 2 ]
graysToWords1 [
	GrayWord1 x, GrayWord1 y, GrayWord1 z, GrayWord1 w,
	GrayWord1 v, GrayWord1 u, GrayWord1 t ] = [
	x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5 .|. w `shiftL` 4 .|.
	v `shiftL` 3 .|. u `shiftL` 2 .|. t `shiftL` 1 ]
graysToWords1 (
	GrayWord1 x : GrayWord1 y : GrayWord1 z : GrayWord1 w :
	GrayWord1 v : GrayWord1 u : GrayWord1 t : GrayWord1 s : gs ) = (
	x `shiftL` 7 .|. y `shiftL` 6 .|. z `shiftL` 5 .|. w `shiftL` 4 .|.
	v `shiftL` 3 .|. u `shiftL` 2 .|. t `shiftL` 1 .|. s ) : graysToWords1 gs

graysToWords2 :: RealFrac d => [Gray d] -> [Word8]
graysToWords2 [] = []
graysToWords2 [GrayWord2 x] = [x `shiftL` 6]
graysToWords2 [GrayWord2 x, GrayWord2 y] = [x `shiftL` 6 .|. y `shiftL` 4]
graysToWords2 [GrayWord2 x, GrayWord2 y, GrayWord2 z] =
	[x `shiftL` 6 .|. y `shiftL` 4 .|. z `shiftL` 2]
graysToWords2 (GrayWord2 x : GrayWord2 y : GrayWord2 z : GrayWord2 w : gs) =
	(x `shiftL` 6 .|. y `shiftL` 4 .|. z `shiftL` 2 .|. w) :
	graysToWords2 gs

graysToWords4 :: RealFrac d => [Gray d] -> [Word8]
graysToWords4 [] = []
graysToWords4 [GrayWord4 g] = [g `shiftL` 4]
graysToWords4 (GrayWord4 x : GrayWord4 y : gs) =
	(x `shiftL` 4 .|. y) : graysToWords4 gs

grayAlphasToWords8 :: RealFrac d => [GrayAlpha d] -> [Word8]
grayAlphasToWords8 [] = []
grayAlphasToWords8 (GrayAlphaWord8 x a : gas) = x : a : grayAlphasToWords8 gas

grayAlphasToWords16 :: RealFrac d => [GrayAlpha d] -> [Word8]
grayAlphasToWords16 [] = []
grayAlphasToWords16 (GrayAlphaWord16 x a : gas) =
	fromIntegral (x `shiftR` 8) : fromIntegral x :
	fromIntegral (a `shiftR` 8) : fromIntegral a : grayAlphasToWords16 gas

encodeRaw :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header -> Maybe Palette ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString ()
encodeRaw nm m hdr mplt ibe obe = void $
-- MAKE IDAT

		do
			bs0 <- Pipe.await
			Unfilter.pngFilter hdr bs0 $ Header.headerToSizes hdr
		Pipe.=$= PipeT.convert BSF.pack
		Pipe.=$= PipeZ.deflate nm m sampleOptions ibe obe
		Pipe.=$= Buffer.format' nm BSF.splitAt' "" 5000

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
					chunkBody = encodePalette plt }

			bd0 <- Pipe.await
			Pipe.yield $ Chunk {
				chunkName = "IDAT",
				chunkBody = bd0 }
			fix \go -> Pipe.awaitMaybe >>= \case
				Nothing -> pure ()
				Just bd -> (>> go) $ Pipe.yield Chunk {
					chunkName = "IDAT", chunkBody = bd }
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

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

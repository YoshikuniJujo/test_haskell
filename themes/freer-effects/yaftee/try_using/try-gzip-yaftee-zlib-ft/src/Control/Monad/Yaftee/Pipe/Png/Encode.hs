{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Encode (

	encodeRgba, encodeGray8

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
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

		Pipe.=$= encodeRaw nm m hdr ibe obe

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
	void $ PipeT.convert ((\(GrayWord8 w) -> w) <$>)
		Pipe.=$= PipeT.convert BSF.pack
		Pipe.=$= encodeRaw nm m hdr ibe obe

encodeRaw :: forall nm m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString BSF.ByteString ()
encodeRaw nm m hdr ibe obe = void $
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

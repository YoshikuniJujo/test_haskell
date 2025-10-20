{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode (

	-- * DECODE

	run_, States, decode, Members,

	-- * DECODE HEADER

	runHeader, StatesHeader, decodeHeader, MembersHeader,

	-- * DECODE PALETTE

	decodePalette

	) where

import Prelude hiding (Monoid)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header qualified as Header

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ

import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Color

run_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run_ = void
	. Buffer.run @nm
	. PipeZ.run @nm
	. flip (State.runN @nm) Header.header0
	. OnDemand.run @nm
	. Chunk.chunkRun_ @nm

type States nm =
	Chunk.ChunkStates nm `Append`
	OnDemand.States nm `Append` '[
		State.Named nm Header.Header,
		State.Named nm (Maybe PipeZ.ByteString),
		State.Named nm (Buffer.Monoid BSF.ByteString) ]

decode :: forall nm m -> (
	PrimBase m, RealFrac d, U.Member Pipe.P es,
	Members nm es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es, U.Base (U.FromFirst m) es ) =>
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString (Either [Rgb d] [Rgba d]) ()
decode nm m ib ob = void $
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "File header error"
		Chunk.chunk nm 500
	Pipe.=$= forever do
		x <- Pipe.await
		Chunk.Chunk { Chunk.chunkName = c } <- State.getN nm
		when (BSF.toStrict c == "IHDR" || BSF.toStrict c == "IDAT")
			$ Pipe.yield x
	Pipe.=$= do
		_ <- OnDemand.onDemand nm Pipe.=$=
			Header.read nm (const $ pure ())
		PipeZ.inflate nm m (Zlib.WindowBitsZlib 15) ib ob
	Pipe.=$= do
		bs0 <- Pipe.await
		rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN nm
		Buffer.format nm BSF.splitAt' bs0 rs
	Pipe.=$= Unfilter.pngUnfilter nm Pipe.=$= bytesToColor nm

type Members nm es = (
	Chunk.ChunkMembers nm es, OnDemand.Members nm es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es )

bytesToColor :: forall nm -> (
	RealFrac d,
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es ) =>
	Eff.E es [Word8] (Either [Rgb d] [Rgba d]) r
bytesToColor nm = do
	bs1 <- Pipe.await
	(ct, bd) <- maybe (Except.throw @String "yet") pure . headerToColorTypeDepth =<< State.getN nm
	case ct of
		Rgb -> do
			Pipe.yield . Left $ pixelsRgb bd bs1
			PipeT.convert (Left . pixelsRgb bd)
		Rgba -> do
			Pipe.yield . Right $ pixelsRgba bd bs1
			PipeT.convert (Right . pixelsRgba bd)

data ColorType = Rgb | Rgba deriving Show
data BitDepth = BitDepth8 | BitDepth16 deriving Show

headerToColorTypeDepth :: Header.Header -> Maybe (ColorType, BitDepth)
headerToColorTypeDepth h = do
	ct <- case Header.headerColorType h of
		Header.ColorTypeColorUsed -> Just Rgb
		Header.ColorType 6 -> Just Rgba
		_ -> Nothing
	bd <- case Header.headerBitDepth h of
		8 -> Just BitDepth8
		16 -> Just BitDepth16
		_ -> Nothing
	pure (ct, bd)

pixelsRgb :: RealFrac d => BitDepth -> [Word8] -> [Rgb d]
pixelsRgb bd bs = case samples bd bs of
	Left ss -> colorsRgb RgbWord8 ss
	Right ss -> colorsRgb RgbWord16 ss

pixelsRgba :: RealFrac d => BitDepth -> [Word8] -> [Rgba d]
pixelsRgba bd bs = case samples bd bs of
	Left ss -> colorsRgba RgbaWord8 ss
	Right ss -> colorsRgba RgbaWord16 ss

samples :: BitDepth -> [Word8] -> Either [Word8] [Word16]
samples BitDepth8 bs = Left bs
samples BitDepth16 [] = Right []
samples BitDepth16 ((fromIntegral -> b) : (fromIntegral -> b') : bs) =
	((b `shiftL` 8 .|. b') :) <$> samples BitDepth16 bs
samples BitDepth16 [_] = error "bad"

colorsRgb :: (s -> s -> s -> Rgb d) -> [s] -> [Rgb d]
colorsRgb rgb = \case
	[] -> []
	r : g : b : ss -> rgb r g b : colorsRgb rgb ss
	_ -> error "bad"

colorsRgba :: (s -> s -> s -> s -> Rgba d) -> [s] -> [Rgba d]
colorsRgba rgba = \case
	[] -> []
	r : g : b : a : ss -> rgba r g b a : colorsRgba rgba ss
	_ -> error "bad"

runHeader :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (StatesHeader nm `Append` es) i o r -> Eff.E es i o Header.Header
runHeader = (snd <$>)
	. flip (State.runN @nm) Header.header0
	. OnDemand.run @nm
	. Chunk.chunkRun_ @nm

type StatesHeader nm =
	Chunk.ChunkStates nm `Append`
	OnDemand.States nm `Append` '[State.Named nm Header.Header]

decodeHeader :: forall nm -> (
	U.Member Pipe.P es,
	MembersHeader nm es,
	U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString o ()
decodeHeader nm = void $
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "File header error"
		Chunk.chunk nm 500
	Pipe.=$= forever do
		x <- Pipe.await
		c <- State.getN nm
		when ("IHDR" `Chunk.isChunkName` c || "IDAT" `Chunk.isChunkName` c)
			$ Pipe.yield x
	Pipe.=$= OnDemand.onDemand nm Pipe.=$= Header.read nm (const $ pure ())

decodePalette :: forall nm -> (
	U.Member Pipe.P es,
	MembersHeader nm es,
	U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString o ()
decodePalette nm = void $
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "File header error"
		Chunk.chunk nm 500
	Pipe.=$= forever do
		x <- Pipe.await
		c <- State.getN nm
		when ("IHDR" `Chunk.isChunkName` c || "IDAT" `Chunk.isChunkName` c)
			$ Pipe.yield x
	Pipe.=$= OnDemand.onDemand nm Pipe.=$= Header.read nm (const $ pure ())

type MembersHeader nm es = (
	Chunk.ChunkMembers nm es, OnDemand.Members nm es,
	U.Member (State.Named nm Header.Header) es )

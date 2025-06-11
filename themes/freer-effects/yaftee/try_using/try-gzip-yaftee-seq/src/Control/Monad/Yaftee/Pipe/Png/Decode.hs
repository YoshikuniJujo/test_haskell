{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode (

	-- * DECODE

	run_, States,
	decode, Members,

	-- * DECODE HEADER

	runHeader, StatesHeader,
	decodeHeader, MembersHeader,

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Word
import Data.Char
import Data.Png
import Data.Png.Header qualified as Header
import Data.Png.Filters

import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib
import Data.TypeLevel.List
import Data.Zlib qualified as Zlib
import Data.Sequence.BitArray qualified as BitArray
import Data.HigherFunctor qualified as HFunctor
import Data.Color
import Data.Bits

run_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run_ = void
	. flip (State.runN @nm) Header.header0 . Chunk.chunkRun_ @nm
	. OnDemand.run_ @nm . Zlib.run_

type States nm =
	Zlib.States nm `Append`
	OnDemand.States nm `Append`
	Chunk.ChunkStates nm `Append` '[
	State.Named nm Header.Header ]

decode :: forall nm -> (
	RealFrac d, U.Member Pipe.P es, Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(Header.Header -> Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Either [Rgb d] [Rgba d]) ()) ->
	(Zlib.Header -> Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) r) ->
	Eff.E es (Seq.Seq Word8) (Either [Rgb d] [Rgba d]) ()
decode nm f g = void $ do
		fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader) $ Except.throw "File header error"
		Chunk.chunk nm 500
	Pipe.=$= do
		_ <- OnDemand.onDemand nm Pipe.=$= Header.read nm f
		rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN nm
		bs <- untilIdat nm
		OnDemand.onDemandWithInitial nm bs Pipe.=$=
			Zlib.decompress nm g rs Pipe.=$=
			pngUnfilter nm Pipe.=$= bytesToColor nm

type Members nm es = (
	Zlib.Members nm es,
	OnDemand.Members nm es,
	Chunk.ChunkMembers nm es,
	U.Member (State.Named nm Header.Header) es )

runHeader :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (StatesHeader nm `Append` es) i o r -> Eff.E es i o Header.Header
runHeader = (snd <$>) . flip (State.runN @nm) Header.header0 . Chunk.chunkRun_ @nm
	. OnDemand.run_ @nm

type StatesHeader nm =
	OnDemand.States nm `Append`
	Chunk.ChunkStates nm `Append` '[
	State.Named nm Header.Header ]

decodeHeader :: forall nm -> (
	U.Member Pipe.P es, MembersHeader nm es,
	U.Member (Except.E String) es ) =>
	Eff.E es (Seq.Seq Word8) o ()
decodeHeader nm = void $ do
		fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader) $ Except.throw "File header error"
		Chunk.chunk nm 500
	Pipe.=$= OnDemand.onDemand nm Pipe.=$= Header.read nm (const $ pure ())

type MembersHeader nm es = (
	OnDemand.Members nm es,
	Chunk.ChunkMembers nm es,
	U.Member (State.Named nm Header.Header) es )

pngUnfilter :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es ) =>
	Eff.E es (Seq.Seq Word8) [Word8] ()
pngUnfilter nm = void do
	bs <- Pipe.await
	h <- State.getN nm
	let	bpp = Header.headerToBpp h
		rbs = Header.headerToRowBytes h
	bs' <- either Except.throw pure
		$ unfilter bpp (replicate rbs 0) bs
	Pipe.yield bs'
	unfilterAll bpp bs'

untilIdat :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk.Chunk) es
	) =>
	Eff.E es (Seq.Seq Word8) o (Seq.Seq Word8)
untilIdat nm = do
	bs <- Pipe.await
	State.getN nm >>= \case
		Chunk.Chunk (seqToString -> "IDAT") -> do
			pure bs
		c -> do
			untilIdat nm

seqToString :: Seq.Seq Word8 -> String
seqToString = toList . (chr . fromIntegral <$>)

unfilterAll :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es
	) =>
	Int -> [Word8] -> Eff.E es (Seq.Seq Word8) [Word8] ()
unfilterAll bpp prior = do
	mbs <- Pipe.awaitMaybe
	case mbs of
		Nothing -> pure ()
		Just Seq.Empty -> pure ()
		Just bs -> do
			bs' <- either Except.throw pure $ unfilter bpp prior bs
			Pipe.yield bs'
			unfilterAll bpp bs'

bytesToColor :: forall nm -> (
	RealFrac d,
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es
	) =>
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

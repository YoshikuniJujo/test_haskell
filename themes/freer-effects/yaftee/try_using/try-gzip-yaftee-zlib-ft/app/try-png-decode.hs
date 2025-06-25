{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (Monoid)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header qualified as Header
import Data.Png.Filters
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ

import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Color

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. flip (State.runN @"foobar") (Monoid ("" :: BSF.ByteString))
		. PipeZ.inflateRun @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			decode @Double "foobar" IO ib ob Pipe.=$= PipeIO.print

	PipeZ.cByteArrayFree ib
	PipeZ.cByteArrayFree ob

decode :: forall nm m -> (
	PrimBase m, RealFrac d, U.Member Pipe.P es,
	Chunk.ChunkMembers nm es, OnDemand.Members nm es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es ) =>
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es BSF.ByteString (Either [Rgb d] [Rgba d]) ()
decode nm m ib ob = void $
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "File header error"
		Chunk.chunk nm 500
	Pipe.=$= forever do
		x <- Pipe.await
		c <- State.getN nm
		when (c == Chunk.Chunk "IHDR" || c == Chunk.Chunk "IDAT")
			$ Pipe.yield x
	Pipe.=$= do
		_ <- OnDemand.onDemand nm Pipe.=$=
			Header.read nm (const $ pure ())
		PipeZ.inflate nm m (Zlib.WindowBitsZlib 15) ib ob
	Pipe.=$= do
		bs0 <- Pipe.await
		rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN nm
		format nm bs0 rs
	Pipe.=$= pngUnfilter nm Pipe.=$= bytesToColor nm

format :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es ) =>
	BSF.ByteString -> [Int] -> Eff.E es BSF.ByteString BSF.ByteString ()
format nm bs0 ns0 = do
	State.putN nm $ Monoid bs0
	($ ns0) $ fix \go -> \case
		[] -> pure ()
		n : ns -> do
			Pipe.yield =<< getInput nm n
			go ns

getInput :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es ) =>
	Int -> Eff.E es BSF.ByteString o BSF.ByteString
getInput nm n = State.getN nm >>= \(Monoid bs) -> case BSF.splitAt' n bs of
	Nothing -> readMore nm >> getInput nm n
	Just (t, d) -> t <$ State.putN nm (Monoid d)

readMore :: forall nm -> (
	Semigroup mono, U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid mono)) es ) =>
	Eff.E es mono o ()
readMore nm =
	Pipe.await >>= \xs -> State.modifyN nm (Monoid . (<> xs) . unMonoid)

newtype Monoid m = Monoid { unMonoid :: m } deriving Show

pngUnfilter :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es BSF.ByteString [Word8] ()
pngUnfilter nm = void do
	bs <- Pipe.await
	h <- State.getN nm
	let	bpp = Header.headerToBpp h
		rbs = Header.headerToRowBytes h
	bs' <- either Except.throw pure
		$ unfilter bpp (replicate rbs 0) bs
	Pipe.yield bs'
	unfilterAll bpp bs'

unfilterAll :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es ) =>
	Int -> [Word8] -> Eff.E es BSF.ByteString [Word8] ()
unfilterAll bpp prior = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just BSF.Empty -> pure ()
	Just bs -> do
		bs' <- either Except.throw pure $ unfilter bpp prior bs
		Pipe.yield bs'
		unfilterAll bpp bs'

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

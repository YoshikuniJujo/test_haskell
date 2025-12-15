{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Apng.Gray1.NoInterlace (writeApngGray1, Frame) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Ratio
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header qualified as H

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Png qualified as Png

import Data.Word
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png qualified as P
import Lifegame.Png.Filter qualified as Filter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Gray1 qualified as G1

import Lifegame.Png.Chunk.Encode qualified as ChunkEn

import Data.Apng qualified as A
import Data.Vector qualified as V

import Data.Word.Crc32 qualified as Crc32
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk

-- EXPORTS

writeApngGray1 :: FilePath -> H.Header -> Int -> Word32 -> [Frame] -> IO ()
writeApngGray1 fp h fn np = writeFG fp h fn np . (toFctlImage <$>) . fromImgs

type Frame = (G1.G, Ratio Word16)

-- CONVERSION: FROM FRAME TO FCTL AND GRAY1

data G = G {
	width :: Word32, height :: Word32, xOffset :: Word32, yOffset :: Word32,
	delay :: Ratio Word16,
	disposeOp :: A.DisposeOp, blendOp :: A.BlendOp,
	image :: V.Vector Word8 }
	deriving Show

toFctlImage :: G -> (A.Fctl, G1.G)
toFctlImage g = (
	A.Fctl {
		A.fctlWidth = w, A.fctlHeight = h,
		A.fctlXOffset = xo, A.fctlYOffset = yo, A.fctlDelay = d,
		A.fctlDisposeOp = dop, A.fctlBlendOp = bop },
	G1.G {
		G1.width = fromIntegral w, G1.height = fromIntegral h,
		G1.body = bd } )
	where G {
		width = w, height = h, xOffset = xo, yOffset = yo,
		delay = d, disposeOp = dop, blendOp = bop, image = bd } = g

fromImgs :: [(G1.G, Ratio Word16)] -> [G]
fromImgs [] = error "no images"
fromImgs ida@((i0, d0) : _) = firstImg i0 d0 : go 0 ida
	where
	go _ [] = error "no images"; go _ [_] = []
	go d ((i1, _) : ids@((i2, d2) : _)) = case fromDiff i1 i2 (d + d2) of
		Nothing -> go (d + d2) ids; Just g -> g : go 0 ids

firstImg :: G1.G -> Ratio Word16 -> G
firstImg G1.G { G1.width = w, G1.height = h, G1.body = bd } dly = G {
	width = fromIntegral w, height = fromIntegral h,
	xOffset = 0, yOffset = 0, delay = dly,
	disposeOp = A.DisposeOpNone, blendOp = A.BlendOpSource, image = bd }

fromDiff :: G1.G -> G1.G -> Ratio Word16 -> Maybe G
fromDiff p c dly = do
	(x, y, G1.G { G1.width = w, G1.height = h, G1.body = b }) <- G1.diff p c
	pure G {
		width = fromIntegral w, height = fromIntegral h,
		xOffset = x, yOffset = y, delay = dly,
		disposeOp = A.DisposeOpNone, blendOp = A.BlendOpSource,
		image = b }

-- WRITE FROM FCTL AND GRAY1 IMAGES

writeFG :: FilePath -> H.Header -> Int -> Word32 -> [(A.Fctl, G1.G)] -> IO ()
writeFG fpp hdr fn np ((unzip . take fn) -> (fctls, imgs)) = do
	chkHdr
	ho <- openFile fpp WriteMode
	(ibe, obe) <- (,) <$> PZ.cByteArrayMalloc 64 <*> PZ.cByteArrayMalloc 64
	hWriteFG ho hdr fn np fctls imgs ibe obe
	PZ.cByteArrayFree ibe; PZ.cByteArrayFree obe; hClose ho
	where
	chkHdr	| H.bitDepth hdr == 1, H.colorType hdr == H.ColorTypeGrayscale,
			H.interlaceMethod hdr == H.InterlaceMethodNon = pure ()
		| otherwise = error "not implemented for such header"

hWriteFG :: Handle -> H.Header -> Int -> Word32 -> [A.Fctl] -> [G1.G] ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> IO ()
hWriteFG h hdr fn np fctls imgs ibe obe = void
	. Eff.runM . Chunk.encodeRun_ @"encode-chunk"
	. Except.run @String . Except.run @Zlib.ReturnCode . Fail.run
	. PipeT.devideRun @"apng" @BSF.ByteString . PZ.run @"apng" . Pipe.run
	$ hWritePipe h hdr fn np fctls imgs ibe obe

-- WRITE PIPE

hWritePipe :: (
	U.Member Pipe.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (State.Named "apng" (PipeT.Devide BSF.ByteString)) es,
	U.Member (State.Named "apng" (Maybe PZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	Handle -> H.Header -> Int -> Word32 -> [A.Fctl] -> [G1.G] ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> Eff.E es i o ()
hWritePipe h hdr fn np fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fctlPixelsGray1 hdr fctls imgs
		Pipe.=$= encode hdr fn np fctls ibe obe
		Pipe.=$= PipeT.convert BSF.toStrict Pipe.=$= PipeBS.hPutStr h

fctlPixelsGray1 :: (
	U.Member Pipe.P es, U.Member U.Fail es, U.Base (U.FromFirst IO) es ) =>
	H.Header -> [A.Fctl] -> [G1.G] -> Eff.E es i A.FctlPixelsGray1 ()
fctlPixelsGray1 _ [] [] = pure ()
fctlPixelsGray1 hdr (fctl : fctls) (img : imgs) =
	Pipe.yield (A.FPG1Fctl fctl) >> fromGrayImage1 img >>
	fctlPixelsGray1 hdr fctls imgs
fctlPixelsGray1 _ _ _ = error "bad"

fromGrayImage1 :: (U.Member Pipe.P es, U.Member U.Fail es) =>
	G1.G -> Eff.E es i A.FctlPixelsGray1 ()
fromGrayImage1 i = case G1.unconsRow i of
	Nothing -> pure ()
	Just (r, j) -> Pipe.yield (A.FPG1Pixels $ toList r) >> fromGrayImage1 j

encode :: (
	A.Fctlable a, Png.Datable a,
	U.Member Pipe.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (State.Named "apng" (PipeT.Devide BSF.ByteString)) es,
	U.Member (State.Named "apng" (Maybe PZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base (U.FromFirst IO) es ) =>
	H.Header -> Int -> Word32 -> [A.Fctl] ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encode _ _ _ [] _ _ = pure ()
encode hdr fn pn fctls@(fctl : _) ibe obe =
	encodeRaw "apng" IO
		hdr { H.width = A.fctlWidth fctl, H.height = A.fctlHeight fctl }
		fn pn (toSize <$> fctls) Nothing ibe obe
	where toSize c = (A.fctlWidth c, A.fctlHeight c)

encodeRaw :: forall nm m -> (
	A.Fctlable a, Png.Datable a, PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (State.Named nm (Maybe PZ.ByteString)) es,
	U.Member (State.Named nm (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Member U.Fail es, U.Base (U.FromFirst m) es ) =>
	H.Header -> Int -> Word32 -> [(Word32, Word32)] -> Maybe P.Palette ->
	PZ.CByteArray (PrimState m) -> PZ.CByteArray (PrimState m) ->
	Eff.E es a BSF.ByteString ()
encodeRaw _ _ _ _ _ [] _ _ _ = pure ()
encodeRaw nm m hdr fn np ((w0, h0) : sz) mplt ibe obe = void $
-- MAKE IDAT
	do	Just fctl <- A.getFctl <$> Pipe.await
		Pipe.yield \sn -> (ChunkEn.Chunk "fcTL" $ A.encodeFctl sn fctl, sn + 1)
		pipeDat nm m False hdr w0 h0 ibe obe
		for_ (take (fn - 1) sz) \(w, h) -> do
			Just fctl' <- A.getFctl <$> Pipe.await
			Pipe.yield \sn -> (ChunkEn.Chunk "fcTL" $ A.encodeFctl sn fctl', sn + 1)
			pipeDat nm m True hdr w h ibe obe
	Pipe.=$= makeChunks hdr fn np mplt

makeChunks :: (
	Integral a, Num b,
	U.Member Pipe.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es
	) =>
	H.Header -> a -> Word32 -> Maybe P.Palette ->
	Eff.E es (b -> (ChunkEn.Chunk, b)) BSF.ByteString ()
makeChunks hdr fn np mplt = void $ do
-- MAKE CHUNKS
		let	bd'' = H.encodeHeader hdr
		Pipe.yield $ \sn -> (
			ChunkEn.Chunk {
				ChunkEn.name = "IHDR",
				ChunkEn.body = BSF.fromStrict bd'' },
				sn )

		case mplt of
			Nothing -> pure ()
			Just plt -> Pipe.yield $ \sn -> (
				ChunkEn.Chunk {
					ChunkEn.name = "PLTE",
					ChunkEn.body = P.encodePalette plt },
				sn )

		Pipe.yield $ \sn -> (
			ChunkEn.Chunk {
				ChunkEn.name = "acTL",
				ChunkEn.body = A.encodeActl $ A.Actl (fromIntegral fn) np },
			sn )

		bd0 <- Pipe.await
		Pipe.yield bd0 -- \sn -> (bd0, sn)
		fix \go -> Pipe.awaitMaybe >>= \case
			Nothing -> pure ()
			Just bd -> (>> go) $ Pipe.yield bd -- \sn -> (bd, sn)

		Pipe.yield \sn -> (
			ChunkEn.Chunk { ChunkEn.name = "IEND", ChunkEn.body = "" },
			sn )
	Pipe.=$= ChunkEn.encode "encode-chunk" 0

pipeDat :: forall nm m -> (
	Png.Datable a,
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PZ.ByteString)) es,
	U.Member (State.Named nm (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Bool ->
	H.Header -> Word32 -> Word32 ->
	PZ.CByteArray (PrimState m) -> PZ.CByteArray (PrimState m) ->
	Eff.E es a (Word32 -> (ChunkEn.Chunk, Word32)) ()
pipeDat nm m iorf hdr w h ibe obe = void $
	(fix \go -> do
		x <- Pipe.await
		if Png.endDat x then pure () else Pipe.yield x >> go)
	Pipe.=$= PipeT.convert (Png.toDat hdr)
	Pipe.=$= Filter.filter hdr (calcSizes hdr w h)
	Pipe.=$= PipeT.convert BSF.pack
	Pipe.=$= PZ.deflate nm m opts ibe obe
	Pipe.=$= PipeT.devide nm BSF.splitAt' "" 1000
--	Pipe.=$= PipeT.devide nm BSF.splitAt' "" 100000
	Pipe.=$= PipeT.convert \dt sn' ->
		(bool ((, sn') . (ChunkEn.Chunk "IDAT")) ((, sn' + 1) . (ChunkEn.Chunk "fdAT" . (BSF.fromBitsBE' sn' <>))) iorf) dt
	where opts = PZ.DeflateOptions {
		PZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
		PZ.deflateOptionsCompressionMethod = Zlib.Deflated,
		PZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
		PZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
		PZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

calcSizes :: H.Header -> Word32 -> Word32 -> [(Int, Int)]
calcSizes H.Header { H.interlaceMethod = H.InterlaceMethodNon } w h =
	[(fromIntegral w, fromIntegral h)]
calcSizes H.Header { H.interlaceMethod = H.InterlaceMethodAdam7 } w h =
	adam7Sizes (fromIntegral w) (fromIntegral h)
calcSizes _ _ _ = error "bad"

adam7Sizes :: Int -> Int -> [(Int, Int)]
adam7Sizes w h = [
	(w `div'` 8, h `div'` 8),
	(w `div'` 4 `div` 2, h `div'` 8),
	(w `div'` 4, h `div'` 4 `div` 2),
	(w `div'` 2 `div` 2, h `div'` 4),
	(w `div'` 2, h `div'` 2 `div` 2),
	(w `div` 2, h `div'` 2), (w, h `div` 2) ]

div' :: Integral n => n -> n -> n
m `div'`n = (m - 1) `div` n + 1

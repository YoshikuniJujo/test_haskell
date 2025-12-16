{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Apng.Gray1.NoInterlace (write, Frame) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as P
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Vector qualified as V
import Data.Ratio
import Data.Bool
import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Image.Gray1 qualified as G1
import Data.Png qualified as Png
import Data.Png.Header qualified as H
import Data.Apng qualified as A
import System.IO
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Lifegame.Png.Chunk.Encode qualified as CEn
import Lifegame.Png.Filter qualified as Filter

-- EXPORTS

write :: FilePath -> H.Header -> Int -> Word32 -> [Frame] -> IO ()
write fp h fn np = writeFG fp h fn np . (toFctlImage <$>) . fromImgs

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
	go d ((i1, _) : ids@((i2, d2) : _)) = case diff i1 i2 (d + d2) of
		Nothing -> go (d + d2) ids; Just g -> g : go 0 ids

firstImg :: G1.G -> Ratio Word16 -> G
firstImg G1.G { G1.width = w, G1.height = h, G1.body = bd } dly = G {
	width = fromIntegral w, height = fromIntegral h,
	xOffset = 0, yOffset = 0, delay = dly,
	disposeOp = A.DisposeOpNone, blendOp = A.BlendOpSource, image = bd }

diff :: G1.G -> G1.G -> Ratio Word16 -> Maybe G
diff p c dly = do
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
	. PipeT.devideRun @"apng" @BSF.ByteString . PZ.run @"apng" . P.run
	$ hWritePipe h hdr fn np fctls imgs ibe obe

-- WRITE PIPE

hWritePipe :: (
	U.Member P.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (State.Named "apng" (PipeT.Devide BSF.ByteString)) es,
	U.Member (State.Named "apng" (Maybe PZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	Handle -> H.Header -> Int -> Word32 -> [A.Fctl] -> [G1.G] ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> Eff.E es i o ()
hWritePipe h hdr fn np fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fctlPixels hdr fctls imgs
		P.=$= encode hdr fn np (toSize <$> fctls) ibe obe
		P.=$= PipeT.convert BSF.toStrict P.=$= PipeBS.hPutStr h
	where toSize c = (A.fctlWidth c, A.fctlHeight c)

fctlPixels :: (U.Member P.P es, U.Member U.Fail es, U.Base IO.I es) =>
	H.Header -> [A.Fctl] -> [G1.G] -> Eff.E es i A.FctlPixelsGray1 ()
fctlPixels _ [] [] = pure ()
fctlPixels h (f : fs) (i : is) =
	P.yield (A.FPG1Fctl f) >> rows i >> fctlPixels h fs is
fctlPixels _ _ _ = error "bad"

rows :: U.Member P.P es =>
	G1.G -> Eff.E es i A.FctlPixelsGray1 ()
rows = fix \go -> maybe (pure ())
	(\(r, i) -> P.yield (A.FPG1Pixels $ toList r) >> go i) . G1.unconsRow

encode :: (
	A.Fctlable a, Png.Datable a, U.Member P.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (State.Named "apng" (Maybe PZ.ByteString)) es,
	U.Member (State.Named "apng" (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	H.Header -> Int -> Word32 -> [(Word32, Word32)] ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encode _ _ _ [] _ _ = pure ()
encode hdr fn np ((w0, h0) : sz) ibe obe = void $
	do	Just f <- A.getFctl <$> P.await
		P.yield \s -> (CEn.C "fcTL" $ A.encodeFctl s f, s + 1)
		dat False hdr w0 h0 ibe obe
		for_ (take (fn - 1) sz) \(w, h) -> do
			Just g <- A.getFctl <$> P.await
			P.yield \s -> (CEn.C "fcTL" $ A.encodeFctl s g, s + 1)
			dat True hdr w h ibe obe
	P.=$= chunks hdr (fromIntegral fn) np

dat :: (
	Png.Datable a, U.Member P.P es,
	U.Member (State.Named "apng" (Maybe PZ.ByteString)) es,
	U.Member (State.Named "apng" (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base IO.I es ) =>
	Bool -> H.Header -> Word32 -> Word32 ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a (Word32 -> (CEn.C, Word32)) ()
dat iof hdr w h ibe obe = void $
	(fix \go -> P.await >>= \x ->
		if Png.endDat x then pure () else P.yield x >> go)
	P.=$= PipeT.convert (Png.toDat hdr)
	P.=$= Filter.filter hdr (calcSizes hdr w h)
	P.=$= PipeT.convert BSF.pack
	P.=$= PZ.deflate "apng" IO opts ibe obe
	P.=$= PipeT.devide "apng" BSF.splitAt' "" 1000
	P.=$= PipeT.convert \dt sn' -> bool
		((, sn') . (CEn.C "IDAT"))
		((, sn' + 1) . (CEn.C "fdAT" . (BSF.fromBitsBE' sn' <>))) iof dt
	where opts = PZ.DeflateOptions {
		PZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
		PZ.deflateOptionsCompressionMethod = Zlib.Deflated,
		PZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
		PZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
		PZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

chunks :: (
	Num b, U.Member P.P es,
	U.Member (State.Named "encode-chunk" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	H.Header ->
	Word32 -> Word32 -> Eff.E es (b -> (CEn.C, b)) BSF.ByteString ()
chunks hdr fn np = void $
	do	P.yield $ (CEn.C { CEn.name = "IHDR", CEn.body = hdr' } ,)
		P.yield $ (CEn.C { CEn.name = "acTL", CEn.body = actl } ,)
		P.yield =<< P.await
		fix \go -> P.awaitMaybe
			>>= \case Nothing -> pure (); Just b -> P.yield b >> go
		P.yield $ (CEn.C { CEn.name = "IEND", CEn.body = "" } ,)
	P.=$= CEn.encode "encode-chunk" 0
	where
	hdr' = BSF.fromStrict $ H.encode hdr
	actl = A.encodeActl $ A.Actl fn np

calcSizes :: H.Header -> Word32 -> Word32 -> [(Int, Int)]
calcSizes H.Header { H.interlaceMethod = H.InterlaceMethodNon } w h =
	[(fromIntegral w, fromIntegral h)]
calcSizes H.Header { H.interlaceMethod = H.InterlaceMethodAdam7 } wdt hgt =
	adam7Sizes (fromIntegral wdt) (fromIntegral hgt)
	where
	adam7Sizes w h = [
		(w `div'` 8, h `div'` 8),
		(w `div'` 4 `div` 2, h `div'` 8),
		(w `div'` 4, h `div'` 4 `div` 2),
		(w `div'` 2 `div` 2, h `div'` 4),
		(w `div'` 2, h `div'` 2 `div` 2),
		(w `div` 2, h `div'` 2), (w, h `div` 2) ]
	m `div'`n = (m - 1) `div` n + 1
calcSizes _ _ _ = error "bad"

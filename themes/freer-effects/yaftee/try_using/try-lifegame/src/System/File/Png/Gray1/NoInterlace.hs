{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Png.Gray1.NoInterlace (
	write
	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header qualified as Header

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Png qualified as Encode
import Control.Monad.Yaftee.Pipe.Tools qualified as Buffer

import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png qualified as Palette
import Lifegame.Png.Filter qualified as Filter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Gray1 qualified as Gray1

import Data.Apng

import Data.Ratio
import Data.Apng qualified as Apng
import Data.Vector qualified as V

write :: FilePath -> Gray1.G -> IO ()
write fp g =
	writeApngGray1Foo fp
		(mkHeader
			(fromIntegral $ Gray1.width g)
			(fromIntegral $ Gray1.height g))
		$ firstImage g 1

mkHeader :: Word32 -> Word32 -> Header.Header
mkHeader w h = Header.Header {
	Header.width = w, Header.height = h,
	Header.bitDepth = 1, Header.colorType = Header.ColorTypeGrayscale,
	Header.compressionMethod = Header.CompressionMethodDeflate,
	Header.filterMethod = Header.FilterMethodDefaultFilter,
	Header.interlaceMethod = Header.InterlaceMethodNon }

writeApngGray1Foo :: FilePath -> Header.Header -> G -> IO ()
writeApngGray1Foo fp hdr = writePngGray1Foo'' fp hdr . toFctlImage

writePngGray1Foo'' :: FilePath -> Header.Header -> (Fctl, Gray1.G) -> IO ()
writePngGray1Foo'' fpp hdr fctlsimg = do
	checkHeader hdr
	putStrLn "writePngGray'' begin"
	ho <- openFile fpp WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	let	(fctl, img_) = fctlsimg

	putStrLn "before hWritePngGray"

	hWritePngGray1' ho hdr (fctl, img_) ibe obe

	putStrLn "after hWritePngGray"

	PipeZ.cByteArrayFree ibe
	PipeZ.cByteArrayFree obe
	hClose ho
	print hdr

checkHeader :: Header.Header -> IO ()
checkHeader hdr
	| Header.bitDepth hdr == 1,
		Header.colorType hdr == Header.ColorTypeGrayscale,
		Header.interlaceMethod hdr == Header.InterlaceMethodNon =
		pure ()
	| otherwise = error "not implemented for such header"

hWritePngGray1' ::
	Handle -> Header.Header -> (Fctl, Gray1.G) ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> IO ()
hWritePngGray1' ho hdr (fctl, img) ibe obe = do
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.run
		. Buffer.devideRun @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipeGray1' ho hdr fctl img ibe obe

hWritePngPipeGray1' :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> Fctl -> Gray1.G ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipeGray1' ho hdr fctl img ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromGrayImage1' img
		Pipe.=$= encodeApngGray1 hdr fctl ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

fromGrayImage1' :: (U.Member Pipe.P es, U.Member U.Fail es) =>
	Gray1.G -> Eff.E es i FctlPixelsGray1 ()
fromGrayImage1' img = case Gray1.unconsRow img of
	Nothing -> pure ()
	Just (r, img') -> do
		Pipe.yield . FPG1Pixels $ toList r
		fromGrayImage1' img'

encodeApngGray1 :: (
	Encode.Datable a,
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Base (U.FromFirst IO) es ) =>
	Header.Header -> Fctl ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encodeApngGray1 hdr fctl ibe obe =
	encodeRawCalcGray1 "barbaz" IO hdr {
		Header.width = fctlWidth fctl,
		Header.height = fctlHeight fctl }
		(fctlToSize fctl)
		Nothing ibe obe

fctlToSize :: Fctl -> (Word32, Word32)
fctlToSize c = (fctlWidth c, fctlHeight c)

encodeRawCalcGray1 :: forall nm m -> (
	Encode.Datable a,
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header ->
	(Word32, Word32) -> Maybe Palette.Palette ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es a BSF.ByteString ()
encodeRawCalcGray1 nm m hdr (w0, h0) mplt ibe obe = void $
-- MAKE IDAT
	pipeDat nm m False hdr w0 h0 ibe obe Pipe.=$= makeChunks hdr mplt

makeChunks :: (
	Num b,
	U.Member Pipe.P es
	) =>
	Header.Header -> Maybe Palette.Palette ->
	Eff.E es (b -> (Chunk, b)) BSF.ByteString ()
makeChunks hdr mplt = void $ do
-- MAKE CHUNKS
		let	bd'' = Header.encode hdr
		Pipe.yield $ \sn -> (
			Chunk {
				chunkName = "IHDR",
				chunkBody = BSF.fromStrict bd'' },
				sn )

		case mplt of
			Nothing -> pure ()
			Just plt -> Pipe.yield $ \sn -> (
				Chunk {
					chunkName = "PLTE",
					chunkBody = Palette.encodePalette plt },
				sn )

		bd0 <- Pipe.await
		Pipe.yield bd0

		Pipe.yield \sn -> (
			Chunk { chunkName = "IEND", chunkBody = "" },
			sn )
	Pipe.=$= do
		Pipe.yield fileHeader
		forever' 0 \sn -> do
			f <- Pipe.await
			let	(c, sn') = f sn
			Pipe.yield $ chunkToByteString c
			pure sn'

fileHeader :: BSF.ByteString
fileHeader = "\x89PNG\r\n\SUB\n"

forever' :: Monad m => a -> (a -> m a) -> m a
forever' st act = act st >>= (`forever'` act)

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
	U.Member (State.Named nm (Buffer.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base (U.FromFirst m) es
	) =>
	Bool ->
	Header.Header -> Word32 -> Word32 ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es a (Word32 -> (Chunk, Word32)) ()
pipeDat nm m iorf hdr w h ibe obe = void $
	(fix \go -> do
		x <- Pipe.await
		if Encode.endDat x then pure () else Pipe.yield x >> go)
	Pipe.=$= PipeT.convert (Encode.toDat hdr)
	Pipe.=$= Filter.filter hdr (calcSizes hdr w h)
	Pipe.=$= PipeT.convert BSF.pack
	Pipe.=$= PipeZ.deflate nm m sampleOptions ibe obe
--	Pipe.=$= Buffer.devide nm BSF.splitAt' "" 1000
	Pipe.=$= Buffer.devide nm BSF.splitAt' "" 100000
	Pipe.=$= PipeT.convert \dt sn' ->
		(bool ((, sn') . (Chunk "IDAT")) ((, sn' + 1) . (Chunk "fdAT" . (BSF.fromBitsBE' sn' <>))) iorf) dt

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

data G = G {
	width :: Word32, height :: Word32,
	xOffset :: Word32, yOffset :: Word32,
	delay :: Ratio Word16,
	disposeOp :: Apng.DisposeOp, blendOp :: Apng.BlendOp,
	image :: V.Vector Word8 }
	deriving Show

toFctlImage :: G -> (Apng.Fctl, Gray1.G)
toFctlImage g = (
	Apng.Fctl {
		Apng.fctlWidth = w, Apng.fctlHeight = h,
		Apng.fctlXOffset = xo, Apng.fctlYOffset = yo,
		Apng.fctlDelay = d,
		Apng.fctlDisposeOp = dop, Apng.fctlBlendOp = bop },
	Gray1.G {
		Gray1.width = fromIntegral w, Gray1.height = fromIntegral h,
		Gray1.body = bd } )
	where
	G {	width = w, height = h, xOffset = xo, yOffset = yo,
		delay = d, disposeOp = dop, blendOp = bop,
		image = bd } = g

firstImage :: Gray1.G -> Ratio Word16 -> G
firstImage Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd } dly =
	G {
		width = fromIntegral w, height = fromIntegral h,
		xOffset = 0, yOffset = 0,
		delay = dly,
		disposeOp = Apng.DisposeOpNone, blendOp = Apng.BlendOpSource, image = bd }

calcSizes :: Header.Header -> Word32 -> Word32 -> [(Int, Int)]
calcSizes Header.Header { Header.interlaceMethod = Header.InterlaceMethodNon } w h =
	[(fromIntegral w, fromIntegral h)]
calcSizes Header.Header { Header.interlaceMethod = Header.InterlaceMethodAdam7 } w h =
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

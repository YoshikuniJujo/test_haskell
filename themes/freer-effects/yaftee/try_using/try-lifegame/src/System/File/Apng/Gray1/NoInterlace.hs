{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Apng.Gray1.NoInterlace (
	writeApngGray1, writeApngGray1',
	writeApngGray1Foo'
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
import Data.Ratio
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF
import Data.Image.Gray qualified as Gray
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Fctl

import Data.Png.Datable qualified as Encode
import Control.Monad.Yaftee.Pipe.Tools qualified as Buffer

import Data.Word
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png.Palette qualified as Palette
import Control.Monad.Yaftee.Pipe.Png.Filter qualified as Unfilter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Gray1 qualified as Gray1

import FctlImage qualified
import FctlImage.Gray1 qualified as FctlImage1
import FctlImageBody.Gray1Words qualified as W
import Tools

import Control.Monad.Yaftee.Pipe.Png.ChunkEncode

import Data.Apng

import Data.Word.Crc32 qualified as Crc32
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as ChunkNew

writeApngGray1' :: FilePath -> Header.Header -> Int -> Word32 -> [(Gray.G, Ratio Word16)] -> IO ()
writeApngGray1' fp hdr fn np = writeApngGray1 fp hdr fn np . FctlImage.fromImagesGray

writeApngGray1Foo' :: FilePath -> Header.Header -> Int -> Word32 -> [(Gray1.G, Ratio Word16)] -> IO ()
writeApngGray1Foo' fp hdr fn np imgs = do
	writeApngGray1Foo fp hdr fn np $ FctlImage1.fromImages imgs
--	print $ FctlImage1.fromImages imgs

writeApngGray1 :: FilePath -> Header.Header -> Int -> Word32 -> [FctlImage.GrayI] -> IO ()
writeApngGray1 fp hdr fn np = writePngGray1'' fp hdr fn np . (FctlImage.toFctlImageGray <$>)

writeApngGray1Foo :: FilePath -> Header.Header -> Int -> Word32 -> [FctlImage1.G] -> IO ()
writeApngGray1Foo fp hdr fn np = writePngGray1Foo'' fp hdr fn np . (FctlImage1.toFctlImage <$>)

writePngGray1'' :: FilePath -> Header.Header -> Int -> Word32 -> [(Fctl, Gray.G)] -> IO ()
writePngGray1'' fpp hdr fn np fctlsimgs = do
	checkHeader hdr
	putStrLn "writePngGray'' begin"
	ho <- openFile fpp WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	let	(fctls, imgs_) = unzip $ take fn fctlsimgs

	putStrLn "before hWritePngGray"

	hWritePngGray1 ho hdr fn np (fctls, imgs_) ibe obe

	putStrLn "after hWritePngGray"

	PipeZ.cByteArrayFree ibe
	PipeZ.cByteArrayFree obe
	hClose ho
	print hdr

writePngGray1Foo'' :: FilePath -> Header.Header -> Int -> Word32 -> [(Fctl, Gray1.G)] -> IO ()
writePngGray1Foo'' fpp hdr fn np fctlsimgs = do
	checkHeader hdr
	putStrLn "writePngGray'' begin"
	ho <- openFile fpp WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	let	(fctls, imgs_) = unzip $ take fn fctlsimgs

	putStrLn "before hWritePngGray"

	hWritePngGray1' ho hdr fn np (fctls, imgs_) ibe obe

	putStrLn "after hWritePngGray"

	PipeZ.cByteArrayFree ibe
	PipeZ.cByteArrayFree obe
	hClose ho
	print hdr

checkHeader :: Header.Header -> IO ()
checkHeader hdr
	| Header.headerBitDepth hdr == 1,
		Header.headerColorType hdr == Header.ColorTypeGrayscale,
		Header.headerInterlaceMethod hdr == Header.InterlaceMethodNon =
		pure ()
	| otherwise = error "not implemented for such header"

hWritePngGray1 ::
	Handle -> Header.Header -> Int -> Word32 -> ([Fctl], [Gray.G]) ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> IO ()
hWritePngGray1 ho hdr fn np (fctls, imgs) ibe obe = do
	void . Eff.runM . ChunkNew.encodeRun_ @"foo" . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.run
		. Buffer.devideRun @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipeGray1 ho hdr fn np fctls imgs ibe obe

hWritePngGray1' ::
	Handle -> Header.Header -> Int -> Word32 -> ([Fctl], [Gray1.G]) ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> IO ()
hWritePngGray1' ho hdr fn np (fctls, imgs) ibe obe = do
	void . Eff.runM . ChunkNew.encodeRun_ @"foo" . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.run
		. Buffer.devideRun @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipeGray1' ho hdr fn np fctls imgs ibe obe

hWritePngPipeGray1 :: (
	U.Member Pipe.P es,
	U.Member (State.Named "foo" Crc32.C) es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> Int -> Word32 -> [Fctl] -> [Gray.G] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipeGray1 ho hdr fn np fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromFctlImagesGray1 IO hdr fctls imgs
		Pipe.=$= encodeApngGray1 hdr fn np fctls ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

hWritePngPipeGray1' :: (
	U.Member Pipe.P es,
	U.Member (State.Named "foo" Crc32.C) es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> Int -> Word32 -> [Fctl] -> [Gray1.G] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipeGray1' ho hdr fn np fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromFctlImagesGray1' hdr fctls imgs
		Pipe.=$= encodeApngGray1 hdr fn np fctls ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

fromFctlImagesGray1 :: forall m -> (
	PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst IO) es ) =>
	Header.Header -> [Fctl] -> [Gray.G] -> Eff.E es i W.BodyGray1 ()
fromFctlImagesGray1 _ _ [] [] = pure ()
fromFctlImagesGray1 m hdr (fctl : fctls) (img : imgs) = do
	Pipe.yield $ W.BodyGray1Fctl fctl
	fromGrayImage1 m fctl img (fctlPoss' hdr fctl)
	fromFctlImagesGray1 m hdr fctls imgs
fromFctlImagesGray1 _ _ _ _ = error "bad"

fromGrayImage1 :: forall m -> (
	PrimMonad m, U.Member Pipe.P es, U.Base (U.FromFirst IO) es ) =>
	Fctl -> Gray.G -> [[(Int, Int)]] -> Eff.E es i W.BodyGray1 ()
fromGrayImage1 m fctl img = \case
	[] -> pure ()
	ps : pss -> do
		Pipe.yield . W.BodyGray1Pixels . boolsToWords
			=<< (\(x, y) -> Eff.effBase . pure @IO . (0x7f <)
				$ Gray.grayRead img x y) `mapM` ps
		fromGrayImage1 m fctl img pss

fromFctlImagesGray1' :: (
	U.Member Pipe.P es, U.Member U.Fail es, U.Base (U.FromFirst IO) es ) =>
	Header.Header -> [Fctl] -> [Gray1.G] -> Eff.E es i W.BodyGray1 ()
fromFctlImagesGray1' _ [] [] = pure ()
fromFctlImagesGray1' hdr (fctl : fctls) (img : imgs) = do
	Pipe.yield $ W.BodyGray1Fctl fctl
	fromGrayImage1' img
	fromFctlImagesGray1' hdr fctls imgs
fromFctlImagesGray1' _ _ _ = error "bad"

fromGrayImage1' :: (U.Member Pipe.P es, U.Member U.Fail es) =>
	Gray1.G -> Eff.E es i W.BodyGray1 ()
fromGrayImage1' img = case Gray1.unconsRow img of
	Nothing -> pure ()
	Just (r, img') -> do
		Pipe.yield . W.BodyGray1Pixels $ toList r
		fromGrayImage1' img'

encodeApngGray1 :: (
	Fctlable a, Encode.Datable a,
	U.Member Pipe.P es,
	U.Member (State.Named "foo" Crc32.C) es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base (U.FromFirst IO) es ) =>
	Header.Header -> Int -> Word32 -> [Fctl] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encodeApngGray1 _ _ _ [] _ _ = pure ()
encodeApngGray1 hdr fn pn fctls@(fctl : _) ibe obe =
	encodeRawCalcGray1 "barbaz" IO hdr {
		Header.headerWidth = fctlWidth fctl,
		Header.headerHeight = fctlHeight fctl } fn pn
		(fctlToSize <$> fctls)
		Nothing ibe obe

fctlToSize :: Fctl -> (Word32, Word32)
fctlToSize c = (fctlWidth c, fctlHeight c)

encodeRawCalcGray1 :: forall nm m -> (
	Fctlable a, Encode.Datable a,
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.Named "foo" Crc32.C) es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Member U.Fail es,
	U.Base (U.FromFirst m) es
	) =>
	Header.Header -> Int -> Word32 ->
	[(Word32, Word32)] -> Maybe Palette.Palette ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es a BSF.ByteString ()
encodeRawCalcGray1 _ _ _ _ _ [] _ _ _ = pure ()
encodeRawCalcGray1 nm m hdr fn np ((w0, h0) : sz) mplt ibe obe = void $
-- MAKE IDAT
	do	Just fctl <- getFctl <$> Pipe.await
		Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl' sn fctl, sn + 1)
		pipeDat nm m False hdr w0 h0 ibe obe
		for_ (take (fn - 1) sz) \(w, h) -> do
			Just fctl' <- getFctl <$> Pipe.await
			Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl' sn fctl', sn + 1)
			pipeDat nm m True hdr w h ibe obe
	Pipe.=$= makeChunks hdr fn np mplt

makeChunks :: (
	Integral a, Num b,
	U.Member Pipe.P es,
	U.Member (State.Named "foo" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es
	) =>
	Header.Header -> a -> Word32 -> Maybe Palette.Palette ->
	Eff.E es (b -> (Chunk, b)) BSF.ByteString ()
makeChunks hdr fn np mplt = void $ do
-- MAKE CHUNKS
		let	bd'' = Header.encodeHeader hdr
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

		Pipe.yield $ \sn -> (
			Chunk {
				chunkName = "acTL",
				chunkBody = encodeActl $ Actl (fromIntegral fn) np },
			sn )

		bd0 <- Pipe.await
		Pipe.yield bd0 -- \sn -> (bd0, sn)
		fix \go -> Pipe.awaitMaybe >>= \case
			Nothing -> pure ()
			Just bd -> (>> go) $ Pipe.yield bd -- \sn -> (bd, sn)

		Pipe.yield \sn -> (
			Chunk { chunkName = "IEND", chunkBody = "" },
			sn )
	Pipe.=$= encode "foo" 0

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
	Pipe.=$= do
		bs0 <- Pipe.await
		Unfilter.pngFilter hdr bs0 $ Header.calcSizes hdr w h
	Pipe.=$= PipeT.convert BSF.pack
	Pipe.=$= PipeZ.deflate nm m sampleOptions ibe obe
	Pipe.=$= Buffer.devide nm BSF.splitAt' "" 1000
--	Pipe.=$= Buffer.devide nm BSF.splitAt' "" 100000
	Pipe.=$= PipeT.convert \dt sn' ->
		(bool ((, sn') . (Chunk "IDAT")) ((, sn' + 1) . (Chunk "fdAT" . (BSF.fromBitsBE' sn' <>))) iorf) dt

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

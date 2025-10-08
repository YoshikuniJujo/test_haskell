{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Apng (writeApngGray) where

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
import Data.Image.Immutable qualified as ImageI
import Data.Png qualified as Png
import Data.Png.Header qualified as Header

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Apng.Decode

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer

import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree.Bits qualified as BSF
import Control.Monad.Yaftee.Pipe.Png.Palette qualified as Palette
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import FctlImage qualified

writeApngGray :: FilePath -> Header.Header -> [FctlImage.GrayI] -> IO ()
writeApngGray fp hdr = writePngGray'' fp hdr . (FctlImage.toFctlImageGray <$>)

writePngGray'' :: FilePath -> Header.Header -> [(Fctl, ImageI.Gray)] -> IO ()
writePngGray'' fpp hdr fctlsimgs = do
	ho <- openFile fpp WriteMode
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64
	let	(fctls, imgs_) = unzip fctlsimgs

	hWritePngGray ho hdr (fctls, imgs_) ibe obe

	PipeZ.cByteArrayFree ibe
	PipeZ.cByteArrayFree obe
	hClose ho

hWritePngGray ::
	Handle -> Header.Header -> ([Fctl], [ImageI.Gray]) ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> IO ()
hWritePngGray ho hdr (fctls, imgs) ibe obe = do
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.run
		. Buffer.run @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipeGray ho hdr fctls imgs ibe obe

hWritePngPipeGray :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> [Fctl] -> [ImageI.Gray] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipeGray ho hdr fctls imgs ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromFctlImagesGray IO hdr fctls imgs
		Pipe.=$= encodeApngGray hdr fctls ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

fromFctlImagesGray :: forall m -> (
	PrimMonad m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst IO) es
	) =>
	Header.Header ->
	[Fctl] ->
	[ImageI.Gray] ->
	Eff.E es i BodyGray ()
fromFctlImagesGray _ _ [] [] = pure ()
fromFctlImagesGray m hdr (fctl : fctls) (img : imgs) = do
	Pipe.yield $ BodyGrayFctl fctl
	fromGrayImage m fctl img (fctlPoss' hdr fctl)
	fromFctlImagesGray m hdr fctls imgs
fromFctlImagesGray _ _ _ _ = error "bad"

fromGrayImage :: forall m -> (
	PrimMonad m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst IO) es ) =>
	Fctl -> ImageI.Gray -> [[(Int, Int)]] -> Eff.E es i BodyGray ()
fromGrayImage m fctl img = \case
	[] -> pure ()
	ps : pss -> do
		Pipe.yield . BodyGrayPixels =<< (\(x, y) -> Eff.effBase . pure @IO $ ImageI.grayRead img x y) `mapM` ps
		fromGrayImage m fctl img pss

encodeApngGray :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es,
	U.Base (U.FromFirst IO) es ) =>
	Header.Header -> [Fctl] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es BodyGray BSF.ByteString ()
encodeApngGray _ [] _ _ = pure ()
encodeApngGray hdr fctls@(fctl : _) ibe obe =
	encodeRawCalcGray "barbaz" IO hdr {
		Header.headerWidth = fctlWidth fctl,
		Header.headerHeight = fctlHeight fctl }
		(fctlToSize <$> fctls)
		Nothing ibe obe

fctlToSize :: Fctl -> (Word32, Word32)
fctlToSize c = (fctlWidth c, fctlHeight c)

encodeRawCalcGray :: forall nm m -> (
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
	Eff.E es BodyGray BSF.ByteString ()
encodeRawCalcGray _ _ _ [] _ _ _ = pure ()
encodeRawCalcGray nm m hdr szs@((w0, h0) : sz) mplt ibe obe = void $
-- MAKE IDAT
 	do	BodyGrayFctl fctl <- Pipe.await
		Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl' sn fctl, sn + 1)
--		Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl fctl, sn + 1)
		pipeDat nm m False hdr w0 h0 ibe obe
		for_ sz \(w, h) -> do
			BodyGrayFctl fctl' <- Pipe.await
			Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl' sn fctl', sn + 1)
--			Pipe.yield \sn -> (Chunk "fcTL" $ encodeFctl fctl', sn + 1)
			pipeDat nm m True hdr w h ibe obe

-- MAKE CHUNKS

	Pipe.=$= do
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
				chunkBody = encodeActl $ Actl (fromIntegral $ length szs) 0 },
			sn )

		bd0 <- Pipe.await
		Pipe.yield bd0 -- \sn -> (bd0, sn)
		fix \go -> Pipe.awaitMaybe >>= \case
			Nothing -> pure ()
			Just bd -> (>> go) $ Pipe.yield bd -- \sn -> (bd, sn)

		Pipe.yield \sn -> (
			Chunk { chunkName = "IEND", chunkBody = "" },
			sn )
	Pipe.=$= do
		Pipe.yield Png.fileHeader
		forever' 0 \sn -> do
			f <- Pipe.await
			let	(c, sn') = f sn
			Pipe.yield $ chunkToByteString c
			pure sn'

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
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
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
	Pipe.=$= Buffer.format' nm BSF.splitAt' "" 1000
--	Pipe.=$= Buffer.format' nm BSF.splitAt' "" 100000
	Pipe.=$= PipeT.convert \dt sn' ->
		(bool ((, sn') . (Chunk "IDAT")) ((, sn' + 1) . (Chunk "fdAT" . (BSF.fromBitsBE' sn' <>))) iorf) dt

sampleOptions :: PipeZ.DeflateOptions
sampleOptions = PipeZ.DeflateOptions {
	PipeZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
	PipeZ.deflateOptionsCompressionMethod = Zlib.Deflated,
	PipeZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
	PipeZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
	PipeZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }

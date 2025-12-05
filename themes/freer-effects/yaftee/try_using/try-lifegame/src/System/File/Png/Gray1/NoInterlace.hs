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
import Data.Png.Header.Data qualified as Header

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Png.Datable qualified as Encode
import Control.Monad.Yaftee.Pipe.Tools qualified as Buffer

import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png.Palette qualified as Palette
import Lifegame.Png.Filter qualified as Filter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Gray1 qualified as Gray1

import FctlImage.Gray1 qualified as FctlImage1
import FctlImageBody.Gray1Words qualified as W

import Data.Apng

write :: FilePath -> Gray1.G -> IO ()
write fp g =
	writeApngGray1Foo fp
		(mkHeader
			(fromIntegral $ Gray1.width g)
			(fromIntegral $ Gray1.height g))
		$ FctlImage1.firstImage g 1

mkHeader :: Word32 -> Word32 -> Header.H
mkHeader w h = Header.H {
	Header.headerWidth = w, Header.headerHeight = h,
	Header.headerBitDepth = 1, Header.headerColorType = Header.ColorTypeGrayscale,
	Header.headerCompressionMethod = Header.CompressionMethodDeflate,
	Header.headerFilterMethod = Header.FilterMethodDefaultFilter,
	Header.headerInterlaceMethod = Header.InterlaceMethodNon }

writeApngGray1Foo :: FilePath -> Header.H -> FctlImage1.G -> IO ()
writeApngGray1Foo fp hdr = writePngGray1Foo'' fp hdr . FctlImage1.toFctlImage

writePngGray1Foo'' :: FilePath -> Header.H -> (Fctl, Gray1.G) -> IO ()
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

checkHeader :: Header.H -> IO ()
checkHeader hdr
	| Header.headerBitDepth hdr == 1,
		Header.headerColorType hdr == Header.ColorTypeGrayscale,
		Header.headerInterlaceMethod hdr == Header.InterlaceMethodNon =
		pure ()
	| otherwise = error "not implemented for such header"

hWritePngGray1' ::
	Handle -> Header.H -> (Fctl, Gray1.G) ->
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
	Handle -> Header.H -> Fctl -> Gray1.G ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipeGray1' ho hdr fctl img ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromGrayImage1' img
		Pipe.=$= encodeApngGray1 hdr fctl ibe obe

		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

fromGrayImage1' :: (U.Member Pipe.P es, U.Member U.Fail es) =>
	Gray1.G -> Eff.E es i W.BodyGray1 ()
fromGrayImage1' img = case Gray1.unconsRow img of
	Nothing -> pure ()
	Just (r, img') -> do
		Pipe.yield . W.BodyGray1Pixels $ toList r
		fromGrayImage1' img'

encodeApngGray1 :: (
	Encode.Datable a,
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Devide BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Base (U.FromFirst IO) es ) =>
	Header.H -> Fctl ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encodeApngGray1 hdr fctl ibe obe =
	encodeRawCalcGray1 "barbaz" IO hdr {
		Header.headerWidth = fctlWidth fctl,
		Header.headerHeight = fctlHeight fctl }
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
	Header.H ->
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
	Header.H -> Maybe Palette.Palette ->
	Eff.E es (b -> (Chunk, b)) BSF.ByteString ()
makeChunks hdr mplt = void $ do
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
	Header.H -> Word32 -> Word32 ->
	PipeZ.CByteArray (PrimState m) -> PipeZ.CByteArray (PrimState m) ->
	Eff.E es a (Word32 -> (Chunk, Word32)) ()
pipeDat nm m iorf hdr w h ibe obe = void $
	(fix \go -> do
		x <- Pipe.await
		if Encode.endDat x then pure () else Pipe.yield x >> go)
	Pipe.=$= PipeT.convert (Encode.toDat hdr)
	Pipe.=$= Filter.filter hdr (Header.calcSizes hdr w h)
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

{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Png.Gray1.NoInterlace (write) where

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
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header qualified as H

import System.IO

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Data.Png qualified as Png

import Data.Word
import Lifegame.Png.Filter qualified as Filter
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Data.Image.Gray1 qualified as G1

import Data.Apng

import Lifegame.Png.Chunk.Encode qualified as ChunkEn

-- WRITE

write :: FilePath -> G1.G -> IO ()
write fp = writeHdr fp <$> header <*> id
	where header g = H.Header {
		H.width = fromIntegral $ G1.width g,
		H.height = fromIntegral $ G1.height g,
		H.bitDepth = 1, H.colorType = H.ColorTypeGrayscale,
		H.compressionMethod = H.CompressionMethodDeflate,
		H.filterMethod = H.FilterMethodDefaultFilter,
		H.interlaceMethod = H.InterlaceMethodNon }

writeHdr :: FilePath -> H.Header -> G1.G -> IO ()
writeHdr fp hdr img = do
	checkHeader
	h <- openFile fp WriteMode
	(ibe, obe) <- (,) <$> PZ.cByteArrayMalloc 64 <*> PZ.cByteArrayMalloc 64
	hWrite h hdr img ibe obe
	PZ.cByteArrayFree ibe; PZ.cByteArrayFree obe; hClose h
	where checkHeader
		| H.bitDepth hdr == 1,
			H.colorType hdr == H.ColorTypeGrayscale,
			H.interlaceMethod hdr == H.InterlaceMethodNon = pure ()
		| otherwise = error "not implemented for such header"

hWrite :: Handle -> H.Header -> G1.G ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> IO ()
hWrite h hdr img ibe obe = void
	. Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
	. Fail.run . ChunkEn.encodeRun_ @"foo"
	. PipeT.devideRun @"png" @BSF.ByteString . PZ.run @"png"
	. P.run $ writePipe h hdr img ibe obe


-- WRITE PIPE

writePipe :: (
	U.Member P.P es,
	ChunkEn.EncodeMembers "foo" es,
	U.Member (State.Named "png" (PipeT.Devide BSF.ByteString)) es,
	U.Member (State.Named "png" (Maybe PZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	Handle -> H.Header -> G1.G ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> Eff.E es i o ()
writePipe h hdr img ibe obe = (`Except.catch` IO.putStrLn) . void $ pixels img
	P.=$= encode hdr (H.width hdr) (H.height hdr) ibe obe
	P.=$= PipeT.convert BSF.toStrict
	P.=$= PipeBS.hPutStr h

pixels :: (U.Member P.P es, U.Member U.Fail es) =>
	G1.G -> Eff.E es i FctlPixelsGray1 ()
pixels = maybe (pure ())
	(\(r, i) -> P.yield (FPG1Pixels $ toList r) >> pixels i) . G1.unconsRow

encode :: (
	Png.Datable a, U.Member P.P es,
	ChunkEn.EncodeMembers "foo" es,
	U.Member (State.Named "png" (Maybe PZ.ByteString)) es,
	U.Member (State.Named "png" (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Member Fail.F es, U.Base IO.I es ) =>
	H.Header -> Word32 -> Word32 ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a BSF.ByteString ()
encode hdr w h ibe obe = void $ idat hdr w h ibe obe P.=$= chunks hdr

idat :: (
	Png.Datable a, U.Member P.P es,
	U.Member (State.Named "png" (Maybe PZ.ByteString)) es,
	U.Member (State.Named "png" (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Member (Except.E String) es,
	U.Base IO.I es ) =>
	H.Header -> Word32 -> Word32 ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a ChunkEn.C ()
idat hdr w h ibe obe = void $
	(fix \go -> P.await >>= \x ->
		if Png.endDat x then pure () else P.yield x >> go)
	P.=$= PipeT.convert (Png.toDat hdr)
	P.=$= (chkHdr >> Filter.filter hdr (fromIntegral w) (fromIntegral h))
	P.=$= PipeT.convert BSF.pack
	P.=$= PZ.deflate "png" IO opts ibe obe
	P.=$= PipeT.devide "png" BSF.splitAt' "" 100000
	P.=$= PipeT.convert (ChunkEn.C "IDAT")
	where
	opts = PZ.DeflateOptions {
		PZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
		PZ.deflateOptionsCompressionMethod = Zlib.Deflated,
		PZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
		PZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
		PZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }
	chkHdr = case hdr of
		H.Header { H.interlaceMethod = H.InterlaceMethodNon } -> pure ()
		_ -> Except.throw @String "not implemented"

chunks :: (
	U.Member P.P es, ChunkEn.EncodeMembers "foo" es,
	U.Member (Except.E String) es, U.Member Fail.F es
	) => H.Header -> Eff.E es ChunkEn.C BSF.ByteString ()
chunks hdr = void $
	do	let	bd'' = H.encode hdr
		P.yield $ ChunkEn.C { ChunkEn.name = "IHDR", ChunkEn.body = BSF.fromStrict bd'' }
		bd0 <- P.await
		P.yield bd0
		P.yield $ ChunkEn.C { ChunkEn.name = "IEND", ChunkEn.body = "" }
	P.=$= ChunkEn.encode "foo"

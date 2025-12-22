{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.File.Png.Gray1.NoInterlace where

import Control.Monad
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
import Data.MonoTraversable
import Data.Function
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Image.Gray1 qualified as G1
import Data.Png qualified as Png
import Data.Png.Header qualified as H
import System.IO
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Lifegame.Png.Chunk.Encode qualified as ChunkEn
import Lifegame.Tools

-- WRITE PIPE

idat :: (
	Png.Datable a, U.Member P.P es,
	U.Member (State.Named "png" (Maybe PZ.ByteString)) es,
	U.Member (State.Named "png" (PipeT.Devide BSF.ByteString)) es,
	U.Member (Except.E Zlib.ReturnCode) es, U.Base IO.I es ) =>
	H.Header -> Word32 ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld ->
	Eff.E es a ChunkEn.C ()
idat hdr h ib ob = void $ PipeT.convert (Png.toDat hdr) P.=$= fltr
	P.=$= PipeT.convert BSF.pack P.=$= PZ.deflate "png" IO opts ib ob
	P.=$= PipeT.devide "png" BSF.splitAt' "" 1000
	P.=$= PipeT.convert (ChunkEn.C "IDAT")
	where
	opts = PZ.DeflateOptions {
		PZ.deflateOptionsCompressionLevel = Zlib.DefaultCompression,
		PZ.deflateOptionsCompressionMethod = Zlib.Deflated,
		PZ.deflateOptionsWindowBits = Zlib.WindowBitsZlib 15,
		PZ.deflateOptionsMemLevel = Zlib.MemLevel 1,
		PZ.deflateOptionsCompressionStrategy = Zlib.DefaultStrategy }
	fltr = fromIntegral h
		`replicateM_` (P.yield . ((0 :) . otoList) =<< P.await)

chunks :: U.Member P.P es => H.Header -> Eff.E es ChunkEn.C ChunkEn.C ()
chunks hdr = void $ do
	P.yield $ ChunkEn.C {
		ChunkEn.name = "IHDR",
		ChunkEn.body = BSF.fromStrict $ H.encode hdr }
	fix \go -> P.awaitMaybe >>= \case
		Nothing -> pure (); Just c -> P.yield c >> go
	P.yield $ ChunkEn.C { ChunkEn.name = "IEND", ChunkEn.body = "" }

writePipe :: (
	U.Member P.P es, ChunkEn.EncodeMembers "png" es,
	U.Member (State.Named "png" (PipeT.Devide BSF.ByteString)) es,
	U.Member (State.Named "png" (Maybe PZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	Handle -> H.Header -> G1.G ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> Eff.E es i o ()
writePipe h hdr img ib ob = (`Except.catch` IO.putStrLn)
	. (`Except.catch` IO.print @Zlib.ReturnCode)
	. void $ rows img
		P.=$= idat hdr (H.height hdr) ib ob P.=$= chunks hdr
		P.=$= ChunkEn.encode "png"
		P.=$= PipeT.convert BSF.toStrict P.=$= PipeBS.hPutStr h

rows :: U.Member P.P es => G1.G -> Eff.E es i Png.PixelsGray1 ()
rows = fix \go -> maybe (pure ())
	(\(r, i) -> P.yield (Png.PG1Pixels $ toList r) >> go i) . G1.unconsRow

-- WRITE

write :: FilePath -> G1.G -> IO ()
write fp = writeHdr fp <$> hdr <*> id
	where hdr g = H.Header {
		H.width = fromIntegral $ G1.width g,
		H.height = fromIntegral $ G1.height g,
		H.bitDepth = 1, H.colorType = H.ColorTypeGrayscale,
		H.compressionMethod = H.CompressionMethodDeflate,
		H.filterMethod = H.FilterMethodDefaultFilter,
		H.interlaceMethod = H.InterlaceMethodNon }

writeHdr :: FilePath -> H.Header -> G1.G -> IO ()
writeHdr fp hdr img = do
	h <- openFile fp WriteMode
	(ib, ob) <- (,) <$> PZ.cByteArrayMalloc 64 <*> PZ.cByteArrayMalloc 64
	hWrite h hdr img ib ob
	PZ.cByteArrayFree ib; PZ.cByteArrayFree ob; hClose h

hWrite :: Handle -> H.Header -> G1.G ->
	PZ.CByteArray RealWorld -> PZ.CByteArray RealWorld -> IO ()
hWrite h hdr img ib ob = void
	. Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode . Fail.run
	. ChunkEn.encodeRun_ @"png" . PipeT.devideRun @"png" @BSF.ByteString
	. PZ.run @"png" . P.run
	$ writePipe h hdr img ib ob


{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.MonoTraversable qualified as PipeMT
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.Int
import Data.ByteString.FingerTree qualified as BSF
import Data.Gzip.Header
import System.IO
import System.Environment

import Data.Word.Crc32 qualified as Crc32
-- import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.Crc32 qualified as PipeCrc32
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- CByteArray.malloc 64
	ob <- CByteArray.malloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode . Fail.runExc id
		. PipeMT.lengthRun @"foobar"
		. PipeZ.inflateRun @"foobar" . PipeCrc32.run @"foobar" . OnDemand.run @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn) . (`Except.catch` IO.print @Zlib.ReturnCode) . void $ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$= OnDemand.onDemand "foobar" Pipe.=$= do
				readHeader "foobar" IO.print
				State.putN "foobar" $ OnDemand.RequestBuffer 25
				do {	rbs <- PipeZ.inflate "foobar" IO (Zlib.WindowBitsRaw 15) ib ob;
					State.putN "foobar" $ OnDemand.RequestPushBack rbs } Pipe.=$=
						PipeMT.length "foobar" Pipe.=$= PipeCrc32.crc32 "foobar"
				"" <- Pipe.await
				State.putN "foobar" $ OnDemand.RequestBytes 4
				PipeCrc32.complement "foobar"
				crc1 <- Crc32.toWord <$> State.getN "foobar"
				crc0 <- Word8.toBits <$> Pipe.await
				when (crc1 /= crc0) $ Except.throw @String "CRC-32 error"
				ln1 <- PipeMT.lengthToInt64 <$> State.getN "foobar"
				ln0 <- Word8.toBits <$> Pipe.await
				when (ln1 /= ln0) $ Except.throw @String "Length error"
			Pipe.=$= PipeT.convert BSF.toStrict Pipe.=$= PipeBS.putStr 

readHeader :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es BSF.ByteString o r) ->
	Eff.E es BSF.ByteString o ()
readHeader nm f = void $ PipeCrc32.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 2
	ids <- Pipe.await
	when (ids /= BSF.pack [31, 139])
		$ Except.throw @String "Bad magic"
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- CompressionMethod . fst . fromJust . BSF.uncons <$> Pipe.await
	Just flgs <- readFlags <$> (fst . fromJust . BSF.uncons <$> Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 4
	mtm <- CTime . Word8.toBits . BSF.toStrict <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ef <-  fst . fromJust . BSF.uncons <$> Pipe.await
	os <- OS <$> (fst . fromJust . BSF.uncons <$> Pipe.await)
	mexflds <- if (flagsRawExtra flgs)
	then do	State.putN nm $ OnDemand.RequestBytes 2
		xlen <- Word8.toBits <$> Pipe.await
		State.putN nm $ OnDemand.RequestBytes xlen
		decodeExtraFields . BSF.toStrict <$> Pipe.await
	else pure []
	State.putN nm OnDemand.RequestString
	mnm <- if flagsRawName flgs
	then Just <$> Pipe.await
	else pure Nothing
	mcmmt <- if flagsRawComment flgs
	then Just <$> Pipe.await
	else pure Nothing
	when (flagsRawHcrc flgs) do
		PipeCrc32.complement nm
		crc <- (.&. 0xffff) . Crc32.toWord <$> State.getN nm
		State.putN nm $ OnDemand.RequestBytes 2
		m <- Word8.toBits <$> Pipe.await
		when (crc /= m) $
			Except.throw @String "Header CRC check failed"
	f GzipHeader {
		gzipHeaderCompressionMethod = cm,
		gzipHeaderFlags = Flags {
			flagsText = flagsRawText flgs,
			flagsHcrc = flagsRawHcrc flgs },
		gzipHeaderModificationTime = mtm,
		gzipHeaderExtraFlags = ef,
		gzipHeaderOperatingSystem = os,
		gzipHeaderExtraField = mexflds,
		gzipHeaderFileName = BSF.toStrict <$> mnm,
		gzipHeaderComment = BSF.toStrict <$> mcmmt }

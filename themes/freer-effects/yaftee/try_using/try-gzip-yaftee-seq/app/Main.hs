{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (head)
import Foreign.C.Types
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Sequence.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.Char
import Data.ByteString qualified as BS
import Data.Gzip.Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	f = IO.print
	void . Eff.runM
		. Except.run @String . Fail.runExc id

		. Deflate.run_ @"foobar"
		. PipeCrc32.run @"foobar"
		. PipeT.lengthRun @"foobar"
		. Pipe.run

		. (`Except.catch` IO.putStrLn) . void $
			PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$= decompress f Pipe.=$=
			forever (((Eff.effBase . putChar . chr . fromIntegral) `mapM`) =<< Pipe.await)

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

decompress :: (
	U.Member Pipe.P es,
	Deflate.Members "foobar" es,
	U.Member (State.Named "foobar" Crc32.C) es,
	U.Member (State.Named "foobar" PipeT.Length) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	(GzipHeader -> Eff.E es (Seq.Seq Word8) (Seq.Seq Word8) r) ->
	Eff.E es (Seq.Seq Word8) (Seq.Seq Word8) ()
decompress f = void $ OnDemand.onDemand "foobar" Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader "foobar" f
	_ <- Deflate.decompress "foobar" Pipe.=$=
		PipeT.convert (either Seq.singleton id) Pipe.=$=
		PipeCrc32.crc32 "foobar" Pipe.=$=
		PipeT.length "foobar"
	PipeCrc32.complement "foobar"
	crc1 <- State.getN "foobar"
	ln1 <- State.getN @PipeT.Length "foobar"
	State.putN "foobar" $ OnDemand.RequestBytes 4
	crc0 <- Crc32.fromWord . Seq.toBits <$> PipeT.skipLeft1
	State.putN "foobar" $ OnDemand.RequestBytes 4
	(ln0 :: PipeT.Length) <- Seq.toBits <$> (Except.getRight @String "bad 2" =<< Pipe.await)
	when (crc1 /= crc0) $ Except.throw @String "CRC error"
	when (ln1 /= ln0) $ Except.throw @String "Length error"

readHeader :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es (Seq.Seq Word8) o r) ->
	Eff.E es (Seq.Seq Word8) o ()
readHeader nm f = void $ PipeCrc32.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 2
	ids <- Pipe.await
	when (ids /= Seq.fromList [31, 139])
		$ Except.throw @String "Bad magic"
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- CompressionMethod <$> (head =<< Pipe.await)
	Just flgs <- readFlags <$> (head =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 4
	mtm <- CTime . Seq.toBits <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ef <-  head =<< Pipe.await
	os <- OS <$> (head =<< Pipe.await)
	mexflds <- if (flagsRawExtra flgs)
	then do	State.putN nm $ OnDemand.RequestBytes 2
		xlen <- Seq.toBits <$> Pipe.await
		State.putN nm $ OnDemand.RequestBytes xlen
		decodeExtraFields . seqToBs <$> Pipe.await
	else pure []
	State.putN nm OnDemand.RequestString
	mnm <- if flagsRawName flgs
	then Just . seqToBs <$> Pipe.await
	else pure Nothing
	mcmmt <- if flagsRawComment flgs
	then Just . seqToBs <$> Pipe.await
	else pure Nothing
	when (flagsRawHcrc flgs) do
		PipeCrc32.complement nm
		crc <- (.&. 0xffff) . Crc32.toWord <$> State.getN nm
		State.putN nm $ OnDemand.RequestBytes 2
		m <- Seq.toBits <$> Pipe.await
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
		gzipHeaderFileName = mnm,
		gzipHeaderComment = mcmmt }

seqToBs :: Seq.Seq Word8 -> BS.ByteString
seqToBs = BS.pack . toList

head :: U.Member (Except.E String) es => Seq.Seq a -> Eff.E es i o a
head = \case
	Seq.Empty -> Except.throw @String "Error: empty Seq"
	x Seq.:<| _ -> pure x

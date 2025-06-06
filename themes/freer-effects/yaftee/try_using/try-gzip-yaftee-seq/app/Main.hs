{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (head)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Word
import Data.ByteString qualified as BS
import Data.Gzip.Header
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Except.run @String . Fail.runExc id . OnDemand.run_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn) . void $ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			OnDemand.onDemand "foobar" Pipe.=$= do
				State.putN "foobar" $ OnDemand.RequestBytes 2
				Right id12 <- Pipe.await
				when (id12 /= identification) $
					Except.throw @String "Gzip identification error"
				State.putN "foobar" $ OnDemand.RequestBuffer 16
				PipeIO.print

identification :: Seq.Seq Word8
identification = Seq.fromList [31, 139]

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

seqToBs :: Seq.Seq Word8 -> BS.ByteString
seqToBs = BS.pack . toList

{-
readHeader :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es i o r) ->
	Eff.E es (Seq.Seq Word8) o ()
readHeader nm f = void $ Crc.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 2
	ids <- Pipe.await
	when (ids /= "\31\139")
		$ Except.throw @String "Bad magic"
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- CompressionMethod <$> (head =<< Pipe.await)
	Just flgs <- readFlags <$> (head =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 4
	mtm <- CTime . LBS.toBits <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ef <-  head =<< Pipe.await
	os <- OS <$> (head =<< Pipe.await)
	mexflds <- if (flagsRawExtra flgs)
	then do	State.putN nm $ OnDemand.RequestBytes 2
		xlen <- LBS.toBits <$> Pipe.await
		State.putN nm $ OnDemand.RequestBytes xlen
		decodeExtraFields . LBS.toStrict <$> Pipe.await
	else pure []
	State.putN nm OnDemand.RequestString
	mnm <- if flagsRawName flgs
	then Just . LBS.toStrict <$> Pipe.await
	else pure Nothing
	mcmmt <- if flagsRawComment flgs
	then Just . LBS.toStrict <$> Pipe.await
	else pure Nothing
	when (flagsRawHcrc flgs) do
		Crc.compCrc32 nm
		crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> State.getN nm
		State.putN nm $ OnDemand.RequestBytes 2
		m <- LBS.toBits <$> Pipe.await
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
		-}

head :: U.Member (Except.E String) es => Seq.Seq a -> Eff.E es i o a
head = \case
	Seq.Empty -> Except.throw @String "Error: empty Seq"
	x Seq.:<| _ -> pure x

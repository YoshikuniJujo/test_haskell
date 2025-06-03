{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Decompress (readHeader) where

import Foreign.C.Types
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.Gzip.Header

readHeader :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es LBS.ByteString o r) ->
	Eff.E es LBS.ByteString o ()
readHeader nm f = void $ Crc.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 2
	ids <- Pipe.await
	when (ids /= "\31\139")
		$ Except.throw @String "Bad magic"
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- CompressionMethod . LBS.head <$> Pipe.await
	Just flgs <- readFlags . LBS.head <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 4
	mtm <- CTime . LBS.toBits <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ef <-  LBS.head <$> Pipe.await
	os <- OS . LBS.head <$> Pipe.await
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

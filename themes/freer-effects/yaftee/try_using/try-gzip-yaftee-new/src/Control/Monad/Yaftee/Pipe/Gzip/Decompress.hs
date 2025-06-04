{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Decompress (run_, decompress) where

import Foreign.C.Types
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.Gzip.Header
import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate

run_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (GzipStates nm `Append` es) i o r -> Eff.E es i o ()
run_ = RunLength.run_
	. Huffman.run . OnDemand.run_ . Crc.runCrc32 . PipeLBS.lengthRun

type GzipStates nm = '[
		State.Named nm PipeLBS.Length,
		State.Named nm Crc.Crc32 ] `Append`
	OnDemand.States nm `Append`
	Huffman.States nm Int `Append`
	RunLength.States nm

decompress :: forall nm -> (
	U.Member Pipe.P es, GzipMembers nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es LBS.ByteString LBS.ByteString r) ->
	Eff.E es LBS.ByteString LBS.ByteString ()
decompress nm f = void $ OnDemand.onDemand nm Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader nm f
	Crc.resetCrc32 nm

	_ <- Deflate.decompress nm Pipe.=$=
		PipeT.convert (either (LBS.pack . (: [])) id)
		Pipe.=$= Crc.crc32 nm Pipe.=$= PipeLBS.length nm

	Crc.compCrc32 nm

	crc <- State.getN nm
	ln <- State.getN nm

	State.putN nm $ OnDemand.RequestBytes 4
	Just crc0 <- Crc.crc32FromByteString <$> PipeT.skipLeft1
	Just ln0 <- PipeLBS.lengthFromByteString 4 <$> (Except.getRight @String "bad 2" =<< Pipe.await)

	when (crc /= crc0) $ Except.throw @String "CRC32 error"
	when (ln /= ln0) $ Except.throw @String "length error"

	Pipe.yield ""

type GzipMembers nm es = (
	RunLength.Members nm es,
	Huffman.Members nm Int es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm PipeLBS.Length) es )

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

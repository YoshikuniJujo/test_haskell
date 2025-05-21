{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Gzip.Decompress (

	Deflate.run_, Deflate.States,

	decompress, Deflate.Members,

	Deflate.BitArray, Deflate.FormatBuffer

	) where

import Foreign.C.Types
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as St
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.Gzip.GzipHeader

import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB

decompress :: forall nm -> (
	U.Member Pipe.P es,
	Deflate.Members nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es BS.ByteString BS.ByteString r) ->
	Eff.E es BS.ByteString BS.ByteString ()
decompress nm phd = void $ OnDemand.onDemand nm Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader nm phd
	Deflate.decompress nm

	Crc.compCrc32 nm
	crc <- St.getN nm
	St.putN nm $ OnDemand.RequestBytes 4
	crc' <- Except.fromJust nvrocc . Crc.byteStringToCrc32 =<< PipeT.skipLeft1
	when (crc /= crc') $ Except.throw @String "bad CRC32"
	ln <- St.getN nm
	ln' <- Except.fromJust nvrocc . PipeB.byteStringToLength
		=<< Except.getRight @String "bad" =<< Pipe.await
	when (ln /= ln') $ Except.throw @String "bad length"

readHeader :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (St.Named nm Crc.Crc32) es,
	U.Member (St.Named nm OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r) ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r1,
		Eff.E es BS.ByteString o r )
readHeader nm f = Crc.crc32 nm Pipe.=$= do
				St.putN nm $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				St.putN nm $ OnDemand.RequestBytes 1
				cm <- (CompressionMethod . BS.head) <$> Pipe.await
				Just flgs <- readFlags . BS.head <$> Pipe.await
				St.putN nm $ OnDemand.RequestBytes 4
				mtm <- CTime . BS.toBits <$> Pipe.await
				St.putN nm $ OnDemand.RequestBytes 1
				ef <- BS.head <$> Pipe.await
				os <- OS . BS.head <$> Pipe.await
				mexflds <- if (flagsRawExtra flgs)
				then do	St.putN nm $ OnDemand.RequestBytes 2
					xlen <- BS.toBits <$> Pipe.await
					St.putN nm $ OnDemand.RequestBytes xlen
					decodeExtraFields <$> Pipe.await
				else pure []
				St.putN nm OnDemand.RequestString
				mnm <- if flagsRawName flgs
				then Just <$> Pipe.await
				else pure Nothing
				mcmmt <- if flagsRawComment flgs
				then Just <$> Pipe.await
				else pure Nothing
				when (flagsRawHcrc flgs) do
					Crc.compCrc32 nm
					crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> St.getN nm
					St.putN nm $ OnDemand.RequestBytes 2
					m <- BS.toBits <$> Pipe.await
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

nvrocc :: String
nvrocc = "Never occur"

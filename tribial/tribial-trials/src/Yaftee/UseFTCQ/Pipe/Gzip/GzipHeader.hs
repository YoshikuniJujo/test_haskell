{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip.GzipHeader (pipeHeader) where

import Foreign.C.Types
import Control.Monad
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.OpenUnion qualified as Union

import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand
import Yaftee.UseFTCQ.Pipe.Crc

import Data.Gzip.GzipHeader

import Tools.ByteStringNum

pipeHeader :: (
	Union.Member Pipe.P es,
	Union.Member (State.S Crc) es, Union.Member (State.S Request) es,
	Union.Member (Except.E String) es, Union.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r') ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r,
		Eff.E es BS.ByteString o r' )
pipeHeader f = crcPipe Pipe.=$= do
	State.put $ RequestBytes 2
	ids <- Pipe.await
	when (ids /= "\31\139") $ Except.throw @String "Bad magic"
	State.put $ RequestBytes 1
	cm <- (CompressionMethod . BS.head) <$> Pipe.await
	Just flgs <- (readFlags . BS.head) <$> Pipe.await
	State.put $ RequestBytes 4
	mtm <- (CTime . bsToNum) <$> Pipe.await
	State.put $ RequestBytes 1
	ef <- BS.head <$> Pipe.await
	os <- OS . BS.head <$> Pipe.await
	mexflds <- if (flagsRawExtra flgs)
	then do
		State.put $ RequestBytes 2
		xlen <- bsToNum <$> Pipe.await
		State.put $ RequestBytes xlen
		exflgs <- decodeExtraFields <$> Pipe.await
		pure exflgs
	else pure []
	State.put RequestString
	mnm <- if (flagsRawName flgs)
	then do	nm <- Pipe.await
		pure $ Just nm
		else pure Nothing
	mcmmt <- if (flagsRawComment flgs)
	then do
		cmmt <- Pipe.await
		pure $ Just cmmt
	else pure Nothing
	when (flagsRawHcrc flgs) do
		compCrc
		crc <- (\(Crc c) -> fromIntegral $ (.&. 0xffff) c) <$> State.get @Crc
		State.put $ RequestBytes 2
		crc0 <- bsToNum @Word16 <$> Pipe.await
		when (crc /= crc0) $
			Except.throw @String "Header CRC check failed"
	f GzipHeader {
		gzipHeaderCompressionMethod = cm,
		gzipHeaderFlags = Flags {
			flagsText = flagsRawText flgs,
			flagsHcrc = flagsRawHcrc flgs },
		gzipHeaderModificationTime = mtm, gzipHeaderExtraFlags = ef,
		gzipHeaderOperatingSystem = os, gzipHeaderExtraField = mexflds,
		gzipHeaderFileName = mnm, gzipHeaderComment = mcmmt }

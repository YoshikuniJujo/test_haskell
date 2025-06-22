{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Maybe
import Data.ByteString.FingerTree qualified as BSF
import Data.Gzip.Header
import System.IO
import System.Environment

import Data.Word.Crc32 qualified as Crc32
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc32

import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			PipeIO.print

{-
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
	when (ids /= BS.pack [31, 139])
		$ Except.throw @String "Bad magic"
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- CompressionMethod . fst . fromJust . BSF.uncons 1 <$> Pipe.await
	Just flgs <- readFlags <$> (head =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 4
	mtm <- CTime . BS.toBits . BSF.toStrict <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ef <-  head =<< Pipe.await
	os <- OS <$> (head =<< Pipe.await)
	mexflds <- if (flagsRawExtra flgs)
	then do	State.putN nm $ OnDemand.RequestBytes 2
		xlen <- BS.toBits <$> Pipe.await
		State.putN nm $ OnDemand.RequestBytes xlen
		decodeExtraFields <$> Pipe.await
	else pure []
	State.putN nm OnDemand.RequestString
	mnm <- if flagsRawName flgs
	then Just <$> Pipe.await
	else pure Nothing
	mcmmt <- if flagsRawComment flgs
	then Just <$> Pipe.await
	else pure Nothing
	when (flagsRawHcrc flgs) do
		PipeCrc32.compCrc32 nm
		crc <- (.&. 0xffff) . Crc32.toWord <$> State.getN nm
		State.putN nm $ OnDemand.RequestBytes 2
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
		-}

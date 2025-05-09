{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.Monad.Yafee.IO qualified as YafeeIO
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.ByteString.OnDemand
import Pipe.Crc
import Pipe.DataCheck

import Gzip
import ByteStringNum

import BitArray qualified as BitArray (B)

import Block

import HuffmanTree
import Pipe.Huffman
import Data.Sequence

import MyEff

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	h' <- openFile "samples/foobarbaz.txt" WriteMode
	(putStrLn . Prelude.take 1000 . show =<<) . runMyEff $
		PipeB.hGet 100 h Pipe.=$= gzipPipe h'
	hClose h'

gzipPipe :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S BitArray.B) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (State.S (Seq Word8)) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (State.Named "format" BS.ByteString) effs,

	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,

	Union.Base IO effs ) =>
	Handle -> Eff.E (Pipe.P BS.ByteString o ': effs) ()
gzipPipe h = onDemand Pipe.=$= do
	YafeeIO.print =<< checkRight Pipe.=$= readHeader

	blocks Pipe.=$= format 100 Pipe.=$= crcPipe Pipe.=$= do
		fix \go -> Pipe.await >>= \case
			Nothing -> pure ()
			Just x -> YafeeIO.print x >> Eff.effBase (BS.hPutStr h x) >> go
		compCrc

	YafeeIO.print . crcToByteString =<< State.get

	State.put $ RequestBytes 4
	Just efoo <- Pipe.await
	YafeeIO.print =<< getRightJust =<< case efoo of
		Left _ -> Pipe.await
		Right _ -> pure $ Just efoo
	YafeeIO.print @Word32 . bsToNum =<< getRightJust =<< Pipe.await

readHeader :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs ) =>
	Eff.E (Pipe.P BS.ByteString o ': effs) GzipHeader
readHeader = crcPipe Pipe.=$= do
	State.put $ RequestBytes 2
	Just ids <- Pipe.await
	when (ids /= "\31\139")
		$ Except.throw @String "Bad magic"
	State.put $ RequestBytes 1
	Just cm <- (CompressionMethod . BS.head <$>) <$> Pipe.await
	Just flgs <- (readFlags . BS.head =<<) <$> Pipe.await
	State.put $ RequestBytes 4
	Just mtm <- (word32ToCTime . bsToNum <$>) <$> Pipe.await @BS.ByteString
	State.put $ RequestBytes 1
	Just ef <- (BS.head <$>) <$> Pipe.await @BS.ByteString
	Just os <- (OS . BS.head <$>) <$> Pipe.await @BS.ByteString
	mexflds <- if (flagsRawExtra flgs)
	then do
		State.put $ RequestBytes 2
		Just xlen <-
			(bsToWord16 <$>) <$> Pipe.await @BS.ByteString
		State.put . RequestBytes $ fromIntegral xlen
		Just exflgs <- (decodeExtraFields <$>) <$> Pipe.await
		pure exflgs
	else pure []
	State.put RequestString
	mnm <- if (flagsRawName flgs)
	then do	nm <- Pipe.await @BS.ByteString
		pure nm
	else pure Nothing
	mcmmt <- if (flagsRawComment flgs)
	then do
		cmmt <- Pipe.await @BS.ByteString
		pure cmmt
	else pure Nothing
	when (flagsRawHcrc flgs) do
		compCrc
		crc <- (\(Crc c) -> fromIntegral $ (.&. 0xffff) c) <$> State.get @Crc
		State.put $ RequestBytes 2
		Just crc0 <- (bsToNum @Word16 <$>) <$>
			Pipe.await @BS.ByteString
		when (crc /= crc0) $
			Except.throw @String "Header CRC check failed"
	pure GzipHeader {
		gzipHeaderCompressionMethod = cm,
		gzipHeaderFlags = Flags {
			flagsText = flagsRawText flgs,
			flagsHcrc = flagsRawHcrc flgs },
		gzipHeaderModificationTime = mtm, gzipHeaderExtraFlags = ef,
		gzipHeaderOperatingSystem = os, gzipHeaderExtraField = mexflds,
		gzipHeaderFileName = mnm, gzipHeaderComment = mcmmt }

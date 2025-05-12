{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress (
	run, hCompress, FileLength ) where

import Control.Monad
import Data.Bool
import Data.ByteString qualified as BS
import Data.Bit qualified as Bit
import Data.Gzip.GzipHeader
import Tools.ByteStringNum
import System.IO

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.Tools qualified as PipeT
import Yaftee.UseFTCQ.Pipe.List qualified as PipeL
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeBS
import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits
import Yaftee.UseFTCQ.Pipe.Crc
import Yaftee.UseFTCQ.Pipe.Gzip.RunLength qualified as RunLength
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple qualified as Triple
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.IO qualified as YIO
import Yaftee.HFunctor qualified as HFunctor
import Yaftee.OpenUnion qualified as U

run :: HFunctor.HFunctor (U.U es) =>
	Eff.E (
		State.S FileLength ':
		State.S Crc ':
		State.S Bit.Queue ':
		State.S Triple.T ':
		State.S BS.ByteString ': es) i o a ->
	Eff.E es i o (((((a, FileLength), Crc), Bit.Queue), Triple.T), BS.ByteString)
run = (`State.run` ("" :: BS.ByteString))
	. (`State.run` Triple.empty)
	. (`State.run` Bit.empty)
	. (`State.run` Crc 0)
	. (`State.run` FileLength 0)

hCompress :: (
	U.Member Pipe.P es,
	U.Member (State.S Crc) es,
	U.Member (State.S FileLength) es,
	U.Member (State.S Bit.Queue) es,
	U.Base YIO.I es
	) =>
	Eff.E es BS.ByteString RunLength.R r -> Handle -> Handle ->
	Eff.E es i o ()
hCompress crl hr ho = void
	$ PipeBS.hGet 500 hr Pipe.=$= compress crl Pipe.=$= PipeBS.hPutStr' ho

compress :: (
	U.Member Pipe.P es,
	U.Member (State.S Bit.Queue) es,
	U.Member (State.S FileLength) es,
	U.Member (State.S Crc) es ) =>
	Eff.E es BS.ByteString RunLength.R r ->
	Eff.E es BS.ByteString BS.ByteString ()
compress crl = void $ lengthPipe Pipe.=$= crcPipe' Pipe.=$= do
	Pipe.yield hdr
	blocks crl
	compCrc
	c <- crcToByteString <$> State.get
	FileLength ln <- State.get
	Pipe.yield c
	Pipe.yield $ numToBs' 4 ln
	where
	hdr = encodeGzipHeader $ sampleGzipHeader { gzipHeaderFileName = Just "OnDemand.hs" }

blocks :: (U.Member Pipe.P es, U.Member (State.S Bit.Queue) es) =>
	Eff.E es i RunLength.R r -> Eff.E es i BS.ByteString ()
blocks crl = void $ crl Pipe.=$=
	PipeL.bundle' 1500 Pipe.=$=
	PipeT.convert'' runLengthsToBits [] Pipe.=$=
	PipeBits.toByteString'

lengthPipe :: (U.Member Pipe.P es, U.Member (State.S FileLength) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
lengthPipe = (Pipe.isMore >>=) . bool (pure ()) $ Pipe.await >>= \bs -> do
	State.modify $ FileLength . (BS.length bs +) . unFileLength
	Pipe.yield bs >> lengthPipe

newtype FileLength = FileLength { unFileLength :: Int } deriving Show

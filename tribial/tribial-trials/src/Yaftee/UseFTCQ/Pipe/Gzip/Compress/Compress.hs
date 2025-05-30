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
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.IO qualified as YIO
import Yaftee.HFunctor qualified as HFunctor
import Yaftee.OpenUnion qualified as U

run :: HFunctor.HFunctor (U.U es) =>
	Eff.E (State.S FileLength ': State.S Crc ': State.S Bit.Queue ': es)
		i o a ->
	Eff.E es i o (((a, FileLength), Crc), Bit.Queue)
run = (`State.run` Bit.empty) . (`State.run` Crc 0) . (`State.run` FileLength 0)

hCompress :: (
	U.Member Pipe.P es,
	U.Member (State.S Crc) es, U.Member (State.S FileLength) es,
	U.Member (State.S Bit.Queue) es, U.Base YIO.I es ) =>
	Eff.E es BS.ByteString RunLength.R r -> GzipHeader ->
	Handle -> Handle -> Eff.E es i o ()
hCompress crl hdr hr ho = void
	$ PipeBS.hGet 100 hr Pipe.=$= compress crl hdr Pipe.=$= PipeBS.hPutStr' ho

compress :: (
	U.Member Pipe.P es,
	U.Member (State.S FileLength) es, U.Member (State.S Crc) es,
	U.Member (State.S Bit.Queue) es ) =>
	Eff.E es BS.ByteString RunLength.R r -> GzipHeader ->
	Eff.E es BS.ByteString BS.ByteString ()
compress crl hdr = void $ lengthPipe Pipe.=$= crcPipe' Pipe.=$= do
	Pipe.yield $ encodeGzipHeader hdr
	blocks crl
	compCrc
	Pipe.yield . crcToByteString =<< State.get
	Pipe.yield . numToBs' 4 . unFileLength =<< State.get

blocks :: (U.Member Pipe.P es, U.Member (State.S Bit.Queue) es) =>
	Eff.E es i RunLength.R r -> Eff.E es i BS.ByteString ()
blocks crl = void $ crl Pipe.=$= PipeL.bundle' 500 Pipe.=$=
	PipeT.convert'' runLengthsToBits [] Pipe.=$= PipeBits.toByteString'

lengthPipe :: (U.Member Pipe.P es, U.Member (State.S FileLength) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
lengthPipe = (Pipe.isMore >>=) . bool (pure ()) $ Pipe.await >>= \bs -> do
	State.modify $ FileLength . (BS.length bs +) . unFileLength
	Pipe.yield bs >> lengthPipe

newtype FileLength = FileLength { unFileLength :: Int } deriving Show

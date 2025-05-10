{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress (
	compressFile',
	RunLengthType
	) where

import Control.Monad
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.IO qualified as YIO
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.Tools qualified as PipeT
import Yaftee.UseFTCQ.Pipe.List qualified as PipeL
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeBS
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.OpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS
import System.IO

import Yaftee.UseFTCQ.Pipe.Gzip.RunLength (RunLength)
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple

import Data.Bit qualified as Bit

import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits

import Yaftee.UseFTCQ.Pipe.Crc
import Tools.ByteStringNum
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.GzipHeader
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.AheadPos

compressFile' :: Eff.E [
	Pipe.P, State.S PipeBits.Queue, State.S FileLength, State.S Crc,
	State.S Triple, State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I] BS.ByteString RunLength r ->
	FilePath -> FilePath -> IO ()
compressFile' crl fp ofp =
	void $ withFile fp ReadMode \hr -> withFile ofp WriteMode \ho ->
		Eff.runM . Fail.run
			. (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
			. (`State.run` triple0)
			. (`State.run` Crc 0)
			. (`State.run` FileLength 0)
			. (`State.run` Bit.empty)
			. Pipe.run
			$ PipeBS.hGet 500 hr Pipe.=$= compress crl Pipe.=$= PipeBS.hPutStr' ho

compress :: (
	Union.Member Pipe.P es,
	Union.Member (State.S Bit.Queue) es,
	Union.Member (State.S FileLength) es,
	Union.Member (State.S Crc) es,
	Union.Base YIO.I es -- FOR DEBUG
	) =>
	Eff.E es BS.ByteString RunLength r ->
	Eff.E es BS.ByteString BS.ByteString ()
compress crl = void $ lengthPipe' Pipe.=$= crcPipe' Pipe.=$= do
	Pipe.yield hdr
	YIO.print ("BEFORE BLOCKS" :: String)
	blocks crl
	YIO.print ("AFTER BLOCKS" :: String)

	compCrc
	c <- crcToByteString <$> State.get
	FileLength ln <- State.get
	Pipe.yield c
	Pipe.yield $ numToBs' 4 ln
	where
	hdr = encodeGzipHeader $ gzipHeaderToRaw sampleGzipHeader { gzipHeaderFileName = Just "OnDemand.hs" }

blocks :: (
	Union.Member Pipe.P es,
	Union.Member (State.S PipeBits.Queue) es,
	Union.Base YIO.I es -- FOR DEBUG
	) =>
	Eff.E es i RunLength r -> Eff.E es i BS.ByteString ()
blocks crl = void $ crl Pipe.=$=
	PipeL.bundle' 1500 Pipe.=$= rlBlock' Pipe.=$= PipeBits.toByteString'

rlBlock' :: (
	Union.Member Pipe.P es,
	Union.Base YIO.I es -- FOR DEBUG
	) => Eff.E es [RunLength] [Bit.B] ()
rlBlock' = PipeT.convert'' runLengthsToBits []

lengthPipe' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S FileLength) effs ) =>
	Eff.E effs BS.ByteString BS.ByteString ()
lengthPipe' = (Pipe.isMore >>=) . bool (pure ())
	$ Pipe.await >>= \bs -> do
		State.modify \(FileLength ln) -> FileLength $ ln + BS.length bs
		Pipe.yield bs
		lengthPipe'

newtype FileLength = FileLength Int deriving Show

type RunLengthType = Eff.E [
	Pipe.P, State.S FileLength, State.S Crc, State.S Triple,
	State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I]

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress (
	compressFile
	) where

import Control.Arrow
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.IO qualified as YIO
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.List qualified as PipeL
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeBS
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Data.Maybe
import Data.Bool
import Data.ByteString qualified as BS
import System.IO

import Yaftee.UseFTCQ.Pipe.Gzip.RunLength (RunLength)
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Triple

import Data.Bit (pattern O)
import Data.Bit qualified as Bit

import Yaftee.UseFTCQ.Pipe.Bits qualified as PipeBits

import Yaftee.UseFTCQ.Pipe.Crc
import Tools.ByteStringNum
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Gzip
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Block
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.AheadPos

compressFile :: RunLengthType BS.ByteString RunLength r -> FilePath -> FilePath -> IO ()
compressFile crl fp ofp = do
	((rl, FileLength fln), cr) <- getRunLengths crl fp
	let	bts = runLengthsToBits rl
		bd = bitsToByteString $ bts ++ [O, O, O, O, O, O, O]
		hdr = encodeGzipHeader $ gzipHeaderToRaw sampleGzipHeader { gzipHeaderFileName = Just "OnDemand.hs" }
	BS.writeFile ofp $
		hdr `BS.append` bd `BS.append`
		crcToByteString cr `BS.append`
		numToBs' 4 fln

getRunLengths :: RunLengthType BS.ByteString RunLength r -> FilePath ->
	IO (([RunLength], FileLength), Crc)
getRunLengths crl fp =
	((fst &&& snd) . fst &&& snd)  . fst . fst . fst . either undefined id <$> tryCompress crl fp

tryCompress ::
	Eff.E '[
		Pipe.P,
		State.S FileLength, State.S Crc,
		State.S Triple, State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I
		] BS.ByteString RunLength r ->
	FilePath -> IO (Either String ((((([RunLength], FileLength), Crc), Triple), BS.ByteString), AheadPos))
tryCompress crl fp = withFile fp ReadMode \h ->
	Eff.runM . Fail.run
		. (`State.run` AheadPos 0) . (`State.run` ("" :: BS.ByteString))
		. (`State.run` triple0)
		. (`State.run` Crc 0)
		. (`State.run` FileLength 0)
		. PipeL.to
		$ PipeBS.hGet 100 h Pipe.=$= lengthPipe Pipe.=$= crcPipe Pipe.=$= crl Pipe.=$= do
			fix \go -> Pipe.isMore >>=
				bool (pure ()) (Pipe.await >>= Pipe.yield >> go)
			compCrc

lengthPipe :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S FileLength) effs ) =>
	Eff.E effs BS.ByteString BS.ByteString ()
lengthPipe = Pipe.await >>= \case
	bs -> do
		State.modify \(FileLength ln) -> FileLength $ ln + BS.length bs
		Pipe.yield bs
		lengthPipe

newtype FileLength = FileLength Int deriving Show

bitsToByteString :: [Bit.B] -> BS.ByteString
bitsToByteString bits = getPure . snd . fromJust . fst
	. Eff.run . (`State.run` Bit.empty) . Pipe.run
	$ Pipe.yield bits Pipe.=$= PipeBits.toByteString Pipe.=$= fix \go ->
		Pipe.isMore
			>>= bool (pure "") ((<$> go) . BS.append =<< Pipe.await)
	where getPure = \case HFreer.Pure x -> x; _ -> error "bad"

type RunLengthType = Eff.E [
	Pipe.P, State.S FileLength, State.S Crc, State.S Triple,
	State.S BS.ByteString, State.S AheadPos, Fail.F, YIO.I]

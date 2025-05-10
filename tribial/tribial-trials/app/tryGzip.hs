{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.IO
import System.Environment
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.IO qualified as PipeI
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeB
import Yaftee.UseFTCQ.Pipe.Gzip.GzipHeader
import Yaftee.UseFTCQ.Pipe.Gzip.Block
import Yaftee.UseFTCQ.Pipe.Gzip.Check
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.UseFTCQ.IO qualified as IO

import Data.Sequence qualified as Seq
import Data.Word
import Data.BitArrayNew qualified as BitArray
import Data.HuffmanTree
import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand
import Yaftee.UseFTCQ.Pipe.Gzip.Huffman

import Yaftee.UseFTCQ.Pipe.Crc qualified as Crc

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Fail.run
		. Except.run @_ @String
		. (flip (State.runN @_ @"format") ("" :: BS.ByteString))
		. (flip (State.runN @_ @"bits") BitArray.empty)
		. (`State.run` ExtraBits 0)
		. (`State.run` (Seq.empty :: Seq.Seq Word8))
		. (`State.run` (fixedTable, fixedTable))
		. (`State.run` Crc.Crc 0)
		. (`State.run` RequestBytes 5)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ PipeB.hGet' 100 h Pipe.=$= onDemand Pipe.=$= do
			_ <- checkRight Pipe.=$= pipeHeader IO.print
			blocks Pipe.=$= format 100 Pipe.=$= Crc.crcPipe Pipe.=$= do
				PipeI.print'
				Crc.compCrc
				IO.print . Crc.crcToByteString =<< State.get

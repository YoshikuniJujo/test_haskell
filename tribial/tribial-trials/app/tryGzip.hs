{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import System.IO
import System.Environment

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.IO qualified as PipeI
import Yaftee.UseFTCQ.Pipe.ByteString qualified as PipeB
import Yaftee.UseFTCQ.Pipe.Gzip
import Yaftee.UseFTCQ.Pipe.Gzip.Check
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.UseFTCQ.IO qualified as IO

import Data.BitArray qualified as BitArray
import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand

import Yaftee.UseFTCQ.Pipe.Crc qualified as Crc

main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Fail.run
		. Except.run @_ @String
		. (`State.run` Crc.Crc 0)
		. (`State.run` RequestBytes 5)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ PipeB.hGet' 100 h Pipe.=$= onDemand Pipe.=$= do
			checkRight Pipe.=$= readMagic' Pipe.=$= PipeI.print
--				IO.print =<< readHeader
--				IO.print =<< readMagic

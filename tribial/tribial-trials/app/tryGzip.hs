{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import System.IO
import System.Environment

import Yaftee.Eff qualified as Eff
import Yaftee.Pipe qualified as Pipe
import Yaftee.Pipe.IO qualified as PipeI
import Yaftee.Pipe.ByteString qualified as PipeB
import Yaftee.Pipe.Gzip
import Yaftee.Pipe.Gzip.Check
import Yaftee.State qualified as State
import Yaftee.Except qualified as Except
import Yaftee.IO qualified as IO

import Data.BitArray qualified as BitArray
import Yaftee.Pipe.ByteString.OnDemand

main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Except.run @_ @String
		. (`State.run` RequestBytes 5)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ PipeB.hGet' 100 h Pipe.=$= onDemand Pipe.=$= do
			checkRight Pipe.=$= do
				IO.print =<< readMagic
--				PipeI.print

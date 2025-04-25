{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeI
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.BitArray qualified as BitArray
import System.IO
import System.Environment

import Pipe.Gzip
import Pipe.ByteString.OnDemand
import Pipe.DataCheck

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. (`State.run` RequestBuffer 64)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ PipeB.hGet' 100 h Pipe.=$= onDemand Pipe.=$= do
			checkRight Pipe.=$= do
				IO.print =<< readMagic

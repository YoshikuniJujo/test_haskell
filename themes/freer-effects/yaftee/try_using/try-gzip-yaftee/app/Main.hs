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
import Data.BitArray qualified as BitArray
import System.IO
import System.Environment

import Pipe.ByteString.OnDemand

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. (`State.run` RequestBuffer 64)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ PipeB.hGet' 100 h Pipe.=$= onDemand Pipe.=$= do
			PipeI.print

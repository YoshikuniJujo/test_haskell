{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import System.IO
import System.Environment

import Yaftee.Eff qualified as Eff
import Yaftee.State qualified as State
import Yaftee.Legacy.NewPipe qualified as Pipe
import Yaftee.Legacy.Pipe.ByteString qualified as PipeB
import Yaftee.Legacy.Pipe.ByteString.OnDemand
import Yaftee.Legacy.Pipe.Gzip qualified as Gzip
import Yaftee.Except qualified as Except
import Yaftee.IO qualified as IO

import Data.BitArray qualified as BitArray

main :: IO ()

main = putStrLn "Slozsoft"

{-
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @_ @String
		. (`State.run` RequestBuffer 64)
		. (`State.run` BitArray.empty)
		. Pipe.run
		$ Pipe.run (PipeB.hGet' 100 h Pipe.=$= onDemand) Pipe.=$= do
--			checkRight Pipe.=$= do
				IO.print =<< Gzip.readMagic
-}

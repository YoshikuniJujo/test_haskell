{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import System.IO
import System.Environment
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress qualified as Compress
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.RunLength qualified as RunLength

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	void $ withFile ifp ReadMode \hr -> withFile ofp WriteMode \ho ->
		Eff.runM . RunLength.run . Compress.run . Pipe.run
			$ Compress.hCompress RunLength.compressRL hr ho

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS
import Data.ByteString.Bit qualified as Bit
import System.IO
import System.Environment

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.Block

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	withFile ifp ReadMode \hr -> withFile ofp WriteMode \ho ->
		Eff.runM
			. (`State.run` Bit.empty)
			. (`State.run` ("" :: BS.ByteString))
			. RunLength.run . PipeL.to
			$ PipeBS.hGet 64 hr Pipe.=$=
				RunLength.compressRL Pipe.=$=
				PipeL.bundle' 500 Pipe.=$=
				PipeT.convert'' runLengthsToBits [] Pipe.=$=
				PipeB.toByteString' Pipe.=$=
				PipeIO.print
	pure ()

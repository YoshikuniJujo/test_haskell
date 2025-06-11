{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Data.Sequence qualified as Seq
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.Runlength.Compress qualified as Runlength

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode

	Eff.runM . Runlength.run_ @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			Runlength.compress "foobar" Pipe.=$=
			PipeIO.print

	hClose h

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

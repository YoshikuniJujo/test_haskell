{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Data.ByteString.Bit qualified as Bit
import System.IO
import System.Environment

import Pipe.RunLength.Compress qualified as RunLength

import Control.Monad.Yaftee.Pipe.Gzip.Compress

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	void $ withFile ifp ReadMode \r -> withFile ofp WriteMode \o -> Eff.runM
		. (`State.run` Bit.empty)
		. PipeBS.lengthRun @"foobar"
		. PipeCrc.runCrc32 @"foobar" . RunLength.run_ . PipeL.to
		$ PipeBS.hGet 64 r Pipe.=$= compress Pipe.=$= PipeBS.hPutStr' o

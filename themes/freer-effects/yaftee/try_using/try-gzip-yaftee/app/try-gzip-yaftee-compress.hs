{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import System.IO
import System.Environment

import Pipe.RunLength.Compress qualified as RunLength

import Control.Monad.Yaftee.Pipe.Gzip.Compress

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Control.HigherOpenUnion qualified as U

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	void $ withFile ifp ReadMode \r -> withFile ofp WriteMode \o -> Eff.runM
		. run_
		. PipeL.to
		$ PipeBS.hGet 64 r Pipe.=$= compress Pipe.=$= PipeBS.hPutStr' o

run_ :: HFunctor.Loose (U.U es) =>
	Eff.E (	Append (RunLength.States "foobar") (
		State.Named "foobar" PipeCrc.Crc32 ':
		State.Named "foobar" PipeBS.Length ':
		State.Named "foobar" PipeB.Queue ':
		es ) ) i o a ->
	Eff.E es i o ()
run_ = void
	. (flip (State.runN @"foobar") PipeB.empty)
	. PipeBS.lengthRun @"foobar"
	. PipeCrc.runCrc32 @"foobar"
	. RunLength.run_ @"foobar"

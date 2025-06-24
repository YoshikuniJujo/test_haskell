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
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$= do
--				OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" IO.print
--				State.putN "foobar" $ OnDemand.RequestBuffer 32
				forever $ Pipe.yield =<< Pipe.await
			Pipe.=$= PipeIO.print

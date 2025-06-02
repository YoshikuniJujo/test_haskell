{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.Gzip.Header
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
			. OnDemand.run_ @"foobar"
			. Pipe.run $
		PipeLBS.hGet 64 h Pipe.=$=
			OnDemand.onDemand "foobar" Pipe.=$= PipeT.checkRight Pipe.=$= do
				State.putN "foobar" $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				State.putN "foobar" $ OnDemand.RequestBytes 1
				IO.print . CompressionMethod . LBS.head =<< Pipe.await
				IO.print . readFlags . LBS.head =<< Pipe.await
				State.putN "foobar" $ OnDemand.RequestBytes 4
				IO.print . CTime . LBS.toBits =<< Pipe.await
				State.putN "foobar" $ OnDemand.RequestBuffer 100
				PipeIO.print

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Decompress

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	f = IO.print
	void . Eff.runM . Except.run @String . Fail.runExc id
			. OnDemand.run_ @"foobar"
			. Crc.runCrc32 @"foobar"
			. Pipe.run $
		PipeLBS.hGet 64 h Pipe.=$=
			OnDemand.onDemand "foobar" Pipe.=$= PipeT.checkRight Pipe.=$= do
				readHeader "foobar" f
				State.putN "foobar" $ OnDemand.RequestBuffer 100
				PipeIO.print

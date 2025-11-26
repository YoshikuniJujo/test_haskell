{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand
	qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.Png.Chunk
import Control.Monad.Yaftee.Except qualified as Except
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. OnDemand.run @"foo" . PipeCrc32.run @"foo" . Pipe.run
		$	PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			decode "foo" Pipe.=$= PipeIO.print

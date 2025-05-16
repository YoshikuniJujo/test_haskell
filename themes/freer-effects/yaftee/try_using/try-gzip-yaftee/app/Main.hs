{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
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
	let	processHeader = IO.hPrint stderr
	void . Eff.runM . Except.run @String . Fail.runExc id
		. OnDemand.run_ . run_ . Pipe.run
		$ PipeB.hGet 64 h Pipe.=$=
			decompress processHeader Pipe.=$= PipeB.putStr'

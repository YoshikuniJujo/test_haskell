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
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Compress

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	void $ withFile ifp ReadMode \r -> withFile ofp WriteMode \o -> Eff.runM
		. run_ @"foobar"
		. PipeL.to
		$ PipeBS.hGet 64 r Pipe.=$= compress "foobar" Pipe.=$= PipeBS.hPutStr' o

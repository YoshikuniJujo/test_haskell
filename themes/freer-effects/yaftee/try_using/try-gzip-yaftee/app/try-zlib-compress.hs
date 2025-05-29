{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Adler32 qualified as Adler32
import Control.Monad.Yaftee.Pipe.Deflate.Compress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import System.Environment

import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

import Pipe.RunLength.Compress qualified as RunLength

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp WriteMode
	_ <- Eff.runM . Deflate.run_ @"foobar" . Adler32.run_ @"barbaz" . Pipe.run
		$ Pipe.yield (BS.concat (replicate 1 "Hello, world!\n")) Pipe.=$= Adler32.adler32 "barbaz" Pipe.=$= do
				Pipe.yield "\x78\x9c"
				Deflate.compress "foobar"
				Pipe.yield . BS.fromBitsBE' =<< State.getsN "barbaz" Adler32.toWord32
			Pipe.=$= PipeBS.hPutStr h
	hClose h

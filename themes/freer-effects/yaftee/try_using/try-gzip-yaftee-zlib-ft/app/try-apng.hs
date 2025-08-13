{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Except qualified as Except
import Data.ByteString.FingerTree qualified as BSF

import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs

	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	print hdr

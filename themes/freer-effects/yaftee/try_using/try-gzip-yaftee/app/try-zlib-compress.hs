{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import System.IO
import System.Environment

import Data.ByteString qualified as BS

import Control.Monad.Yaftee.Pipe.Zlib.Compress qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp WriteMode
	_ <- Eff.runM . Zlib.run @"foobar" . Pipe.run
		$ Pipe.yield (BS.concat (replicate 1 "Hello, world!\n")) Pipe.=$=
			Zlib.compress "foobar" Pipe.=$= PipeBS.hPutStr h
	hClose h

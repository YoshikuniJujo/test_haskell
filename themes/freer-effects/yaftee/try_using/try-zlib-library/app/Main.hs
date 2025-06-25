{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import System.Environment

import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Advanced.Core

import Data.ByteString.FingerTree qualified as BSF

import Control.Monad.Yaftee.Pipe.Zlib qualified as Zlib

inputBufSize, outputBufSize :: Int
inputBufSize = 64 * 4
outputBufSize = 64

readBufSize :: Int
readBufSize = 64 * 5

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	(hi, ho) <- (,) <$> openFile fpi ReadMode <*> openFile fpo WriteMode
	(i, o) <- (,)
		<$> Zlib.cByteArrayMalloc inputBufSize
		<*> Zlib.cByteArrayMalloc outputBufSize

	void . Eff.runM . Except.run @ReturnCode . Zlib.inflateRun @"foobar" . Pipe.run
		. (`Except.catch` IO.print @ReturnCode) . void
		$ PipeBS.hGet readBufSize hi Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Zlib.inflate "foobar" IO (WindowBitsZlibAndGzip 15) i o Pipe.=$=
			PipeT.convert BSF.toStrict Pipe.=$=
			PipeBS.hPutStr ho

	Zlib.cByteArrayFree i; Zlib.cByteArrayFree o
	hClose ho; hClose hi

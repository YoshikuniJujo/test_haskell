{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (Monoid)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Png.run_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode) . void
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Png.decode @Double "foobar" IO ib ob Pipe.=$= PipeIO.print

	PipeZ.cByteArrayFree ib
	PipeZ.cByteArrayFree ob

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pp
import Control.Monad.Yaftee.Pipe.Tools qualified as PpT
import Control.Monad.Yaftee.Pipe.IO qualified as PpIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PpBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as Z
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment
import Codec.Compression.Zlib.Constant.Core qualified as Z
import Codec.Compression.Zlib.Advanced.Core qualified as Z

readBufSize :: Int
readBufSize = 64 * 5

inputBufSize, outputBufSize :: Int
inputBufSize = 64 * 4
outputBufSize = 64

main :: IO ()
main = do
	fp : _ <- getArgs; h <- openFile fp ReadMode
	(i, o) <- (,)
		<$> Z.cByteArrayMalloc inputBufSize
		<*> Z.cByteArrayMalloc outputBufSize

	void . Eff.runM . Except.run @Z.ReturnCode . Z.run @"" . Pp.run
		. (`Except.catch` IO.print @Z.ReturnCode) . void
		$ PpBS.hGet readBufSize h Pp.=$=
			PpT.convert BSF.fromStrict Pp.=$=
			Z.inflate "" IO (Z.WindowBitsZlibAndGzip 15) i o Pp.=$=
			PpIO.print

	Z.cByteArrayFree i; Z.cByteArrayFree o; hClose h

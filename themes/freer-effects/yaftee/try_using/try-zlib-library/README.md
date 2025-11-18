# try-zlib-library

sample1

```Haskell
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
```

sample2

```Haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pp
import Control.Monad.Yaftee.Pipe.Tools qualified as PpT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PpBS
import Control.Monad.Yaftee.Pipe.Zlib qualified as Z
import Control.Monad.Yaftee.Except qualified as Except
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
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
	fp : _ <- getArgs; cnt <- BS.readFile fp
	either print BS.putStr $ runST $ inflate cnt

inflate :: forall m . PrimBase m =>
	BS.ByteString -> m (Either Z.ReturnCode BS.ByteString)
inflate s = do
	(i, o) <- (,)	<$> Z.cByteArrayMalloc inputBufSize
			<*> Z.cByteArrayMalloc outputBufSize
	r <- Eff.runM . Except.run @Z.ReturnCode . Z.run @"" . PpBS.to
		$ PpBS.from readBufSize s Pp.=$=
			PpT.convert BSF.fromStrict Pp.=$=
			Z.inflate "" m (Z.WindowBitsZlibAndGzip 15) i o Pp.=$=
			PpT.convert BSF.toStrict
	r <$ (Z.cByteArrayFree i >> Z.cByteArrayFree o)
```

sample3

```Haskell
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
import Control.Monad.Yaftee.Pipe.Zlib qualified as PpZ
import Control.Monad.Yaftee.Except qualified as Ex
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment
import Codec.Compression.Zlib.Constant.Core qualified as Z
import Codec.Compression.Zlib.Advanced.Core qualified as Z

main :: IO ()
main = do
	fp : as <- getArgs; h <- openFile fp ReadMode
	let	opt = case as of
			[] -> options $ Z.WindowBitsRaw 15
			["zlib"] -> options $ Z.WindowBitsZlib 15
			["gzip"] -> options $ Z.WindowBitsGzip 15
			_ -> error "bad arguments"
	ib <- PpZ.cByteArrayMalloc 64; ob <- PpZ.cByteArrayMalloc 64
	_ <- Eff.runM . Ex.run @Z.ReturnCode . PpZ.run @"" . Pp.run
		. (`Ex.catch` IO.print @Z.ReturnCode) . void $ PpBS.hGet 32 h
			Pp.=$= PpT.convert BSF.fromStrict
			Pp.=$= PpZ.deflate "" IO opt ib ob
			Pp.=$= PpIO.print
	PpZ.cByteArrayFree ib; PpZ.cByteArrayFree ob; hClose h

options :: Z.WindowBits -> PpZ.DeflateOptions
options wb = PpZ.DeflateOptions {
	PpZ.deflateOptionsCompressionLevel = Z.DefaultCompression,
	PpZ.deflateOptionsCompressionMethod = Z.Deflated,
	PpZ.deflateOptionsWindowBits = wb,
	PpZ.deflateOptionsMemLevel = Z.MemLevel 8,
	PpZ.deflateOptionsCompressionStrategy = Z.DefaultStrategy }
```

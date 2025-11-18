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
	r <- ((fst <$>) <$>)
		. Eff.runM . Except.run @Z.ReturnCode . Z.run @"" . PpBS.to
		$ PpBS.from readBufSize s Pp.=$=
			PpT.convert BSF.fromStrict Pp.=$=
			Z.inflate "" m (Z.WindowBitsZlibAndGzip 15) i o Pp.=$=
			PpT.convert BSF.toStrict
	r <$ (Z.cByteArrayFree i >> Z.cByteArrayFree o)

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.ByteString qualified as BS
import System.Environment

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Basic.Core
import Codec.Compression.Zlib.Advanced.Core

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	BS.useAsCStringLen cnt \(p, ln) -> allocaBytes (64 * 64) \o -> do
		strm <- streamThaw streamInitial {
			streamNextIn = castPtr p,
			streamAvailIn = fromIntegral ln,
			streamNextOut = o,
			streamAvailOut = 64 * 64 }
		print =<< withStreamPtr strm \s ->
			c_inflateInit2 s $ WindowBitsZlibAndGzip 15
		print =<< withStreamPtr strm \s -> c_inflate s Finish
		print =<< withStreamPtr strm c_inflateEnd
		ao <- availOut strm
		BS.putStr =<< BS.packCStringLen
			(castPtr o, 64 * 64 - fromIntegral ao)

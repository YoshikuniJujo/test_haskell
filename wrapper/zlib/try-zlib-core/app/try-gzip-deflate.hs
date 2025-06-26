{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

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
	fpi : fpo : _ <- getArgs
	cnt <- BS.readFile fpi
	BS.useAsCStringLen cnt \(p, ln) -> allocaBytes (64 * 64) \o -> do
		strm <- streamThaw streamInitial {
			streamNextIn = castPtr p,
			streamAvailIn = fromIntegral ln,
			streamNextOut = o,
			streamAvailOut = 64 * 64 }
		print =<< deflateInit2 strm
			DefaultCompression Deflated (WindowBitsGzip 15)
			(MemLevel 8) DefaultStrategy
		print =<< deflate strm Finish
		print =<< deflateEnd strm
		strm' <- streamFreeze strm
		print strm'
		BS.writeFile fpo =<< BS.packCStringLen (
			castPtr o,
			64 * 64 - fromIntegral (streamAvailOut strm') )

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
		print =<< withStreamPtr strm \s -> c_deflateInit s DefaultCompression
		print =<< withStreamPtr strm \s -> c_deflate s Finish
		print =<< withStreamPtr strm c_deflateEnd
		strm' <- streamFreeze strm
		print strm'
		BS.writeFile fpo =<< BS.packCStringLen (
			castPtr o,
			64 * 64 - fromIntegral (streamAvailOut strm') )

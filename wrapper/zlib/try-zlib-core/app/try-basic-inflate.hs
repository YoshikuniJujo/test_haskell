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
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	BS.useAsCStringLen cnt \(p, ln) -> allocaBytes (64 * 64) \o -> do
		strm <- streamThaw streamInitial {
			streamNextIn = castPtr p,
			streamAvailIn = fromIntegral ln,
			streamNextOut = o,
			streamAvailOut = 64 * 64 }
		print =<< inflateInit strm
		print =<< inflate strm Finish
		print =<< inflateEnd strm
		strm' <- streamFreeze strm
		print strm'
		BS.putStr =<< BS.packCStringLen (castPtr o, 64 * 64)

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.ByteString qualified as BS
import System.Environment
import Tools

import Codec.Compression.Zlib.Utility.Core

main :: IO ()
main = do
	fpi : _ <- getArgs
	bsi <- BS.readFile fpi
	let	ln, ln' :: Integral n => n
		ln = fromIntegral $ BS.length bsi
		ln' = fromIntegral $ c_compressBound ln
	bsToPtrW8 bsi \pbsi _ -> alloca \pln' -> allocaArray ln' \pbso -> do
		poke pln' ln'
		print =<< c_compress pbso pln' pbsi ln
		BS.writeFile (fpi ++ ".zlib") =<< ptrW8ToBs pbso . fromIntegral =<< peek pln'

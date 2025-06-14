{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.ByteString qualified as BS

import Codec.Compression.Zlib.Utility.Core

import Tools

main :: IO ()
main = do
	let	ln, ln' :: Integral n => n
		ln = fromIntegral $ BS.length hello
		ln' = fromIntegral $ c_compressBound ln
	bsToPtrW8 hello \ph _ -> alloca \dln ->
		allocaArray ln' \ph' -> alloca \dln' ->
		allocaArray ln \ph'' -> do
		poke dln' ln'
		print =<< c_compress ph' dln' ph ln
		ln'' <- peek dln'
		print ln''
		poke dln ln
		print =<< c_uncompress ph'' dln ph' ln''
		print =<< ptrW8ToBs ph'' . fromIntegral =<< peek dln

hello :: BS.ByteString
hello = "Hello, world!"

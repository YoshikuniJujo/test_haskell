{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Word
import Data.ByteString qualified as BS
import System.Environment

import Codec.Compression.Zlib.Utility.Core

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
		

bsToPtrW8 :: BS.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
bsToPtrW8 bs a = do
	let	ws = BS.unpack bs
		ln = BS.length bs
	allocaArray ln \p -> do
		pokeArray p ws
		a p ln

ptrW8ToBs :: Ptr Word8 -> Int -> IO BS.ByteString
ptrW8ToBs p ln = BS.pack <$> peekArray ln p
	
hello :: BS.ByteString
hello = "Hello, world!"

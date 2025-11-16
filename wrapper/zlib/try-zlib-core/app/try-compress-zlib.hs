{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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
	fpi : _ <- getArgs
	bsi <- BS.readFile fpi
	let	ln, ln' :: Integral n => n
		ln = fromIntegral $ BS.length bsi
		ln' = fromIntegral $ c_compressBound ln
	bsToPtrW8 bsi \pbsi _ -> alloca \pln' -> allocaArray ln' \pbso -> do
		poke pln' ln'
		print =<< c_compress pbso pln' pbsi ln
		BS.writeFile (fpi ++ ".zlib") =<< ptrW8ToBs pbso . fromIntegral =<< peek pln'

bsToPtrW8 :: BS.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
bsToPtrW8 bs a = do
	let	ws = BS.unpack bs
		ln = BS.length bs
	allocaArray ln \p -> do
		pokeArray p ws
		a p ln

ptrW8ToBs :: Ptr Word8 -> Int -> IO BS.ByteString
ptrW8ToBs p ln = BS.pack <$> peekArray ln p

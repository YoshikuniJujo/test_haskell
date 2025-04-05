{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Gzip
import System.Environment

sample0 :: FilePath
sample0 = "samples/abcd.txt.gz"

main :: IO ()
main = do
	fp : cont : _ <- getArgs
	BS.writeFile fp . mkNonCompressed $ BSC.pack (cont ++ "\n")

mkNonCompressed :: BS.ByteString -> BS.ByteString
mkNonCompressed cont = do
	(encodeGzipHeader sampleGzipHeader `BS.snoc` 0x01) `BS.append`
		word16ToByteStringPair (fromIntegral $ BS.length cont) `BS.append`
		cont `BS.append`
		crc' cont `BS.append`
		word32ToByteString (fromIntegral $ BS.length cont)

word32ToByteString :: Word32 -> BS.ByteString
word32ToByteString w = BS.pack $ fromIntegral <$> [b0, b1, b2, b3]
	where
	b0 = w .&. 0xff
	b1 = w `shiftR` 8 .&. 0xff
	b2 = w `shiftR` 16 .&. 0xff
	b3 = w `shiftR` 24

word16ToByteStringPair :: Word16 -> BS.ByteString
word16ToByteStringPair w = BS.pack $ fromIntegral <$> [b0, b1, b2, b3]
	where
	b0 = w .&. 0xff
	b1 = w `shiftR` 8
	b2 = complement b0
	b3 = complement b1

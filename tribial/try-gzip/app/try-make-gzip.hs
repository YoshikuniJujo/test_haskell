{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ByteString qualified as BS
import Gzip

sample0 :: FilePath
sample0 = "samples/abcd.txt.gz"

main :: IO ()
main = do
	print =<< BS.readFile sample0
	let	rslt = (encodeGzipHeader sampleGzipHeader `BS.snoc` 0x01) `BS.append`
			"\x04\x00\xfb\xff\&abcd" `BS.append`
			crc' "abcd" `BS.append`
			"\x04\x00\x00\x00"
	print rslt
	BS.writeFile "samples/foo.txt.gz" rslt

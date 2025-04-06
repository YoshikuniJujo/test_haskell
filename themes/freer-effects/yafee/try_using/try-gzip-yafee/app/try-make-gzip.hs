{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Gzip

import Control.Monad.IO.Class
import Options.Declarative

sample0 :: FilePath
sample0 = "samples/abcd.txt.gz"

main :: IO ()
main = run_ realMain

realMain ::
	Flag "n" '["file-name"] "STRING" "source file name" (Maybe String) ->
	Arg "Output file" String -> Arg "Contents" String ->
	Cmd "Make gzip file" ()
realMain mfn fp cont = liftIO do
	BS.writeFile (get fp) . mkNonCompressed (get mfn) $ BSC.pack (get cont ++ "\n")

mkNonCompressed :: Maybe String -> BS.ByteString -> BS.ByteString
mkNonCompressed mfn cont =
	(encodeGzipHeader
		(gzipHeaderToRaw sampleGzipHeader { gzipHeaderFileName = BSC.pack <$> mfn }) `BS.snoc` 0x01) `BS.append`
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

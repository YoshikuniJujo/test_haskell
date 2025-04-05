{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gzip where

import Foreign.C.Types
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Crc

import ByteStringNum

ids0 :: BS.ByteString
ids0 = "\x1f\x8b"

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime, -- Word32,
--	gzipHeaderModificationTime :: Word32,
	gzipExtraFlags :: Word8,
	gzipOperatingSystem :: OS,
	gzipFileName :: BS.ByteString }
	deriving Show

word32ToCTime :: Word32 -> CTime
word32ToCTime = CTime . fromIntegral

cTimeToWord32 :: CTime -> Word32
cTimeToWord32 (CTime w) = fromIntegral w

newtype CompressionMethod = CompressionMethod {
	unCompressionMethod :: Word8 }

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 8

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show cm = "(CompressionMethod " ++ show cm ++ ")"

newtype OS = OS { unOS :: Word8 }

pattern OSUnix :: OS
pattern OSUnix = OS 3

instance Show OS where
	show OSUnix = "OSUnix"
	show os = "(OS " ++ show os ++ ")"

readFlags :: Word8 -> Maybe Flags
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just Flags {
		flagsText = w `testBit` 0,
		flagsHcrc = w `testBit` 1,
		flagsExtra = w `testBit` 2,
		flagsName = w `testBit` 3,
		flagsComment = w `testBit` 4 }

encodeFlags :: Flags -> Word8
encodeFlags fs = foldl setBit 0 xs
	where
	xs = map fst . filter snd . zip [0 ..] $ ($ fs) <$>
		[flagsText, flagsHcrc, flagsExtra, flagsName, flagsComment]

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool,
	flagsExtra :: Bool,
	flagsName :: Bool,
	flagsComment :: Bool }
	deriving Show

encodeGzipHeader :: GzipHeader -> BS.ByteString
encodeGzipHeader hdr = ids0 `BS.append`
	(unCompressionMethod (gzipHeaderCompressionMethod hdr) `BS.cons`
		encodeFlags (gzipHeaderFlags hdr) `BS.cons`
		numToBs (cTimeToWord32 $ gzipHeaderModificationTime hdr)) `BS.append`
	(gzipExtraFlags hdr `BS.cons`
		unOS (gzipOperatingSystem hdr) `BS.cons`
		gzipFileName hdr) `BS.snoc`
	0

sampleGzipHeader :: GzipHeader
sampleGzipHeader = GzipHeader {
	gzipHeaderCompressionMethod = CompressionMethodDeflate,
	gzipHeaderFlags = Flags {
		flagsText = False,
		flagsHcrc = False,
		flagsExtra = False,
		flagsName = True,
		flagsComment = False },
	gzipHeaderModificationTime = 1743055415,
	gzipExtraFlags = 0,
	gzipOperatingSystem = OSUnix,
	gzipFileName = "abcd.txt" }

crc' :: BS.ByteString -> BS.ByteString
crc' = numToBs . crc

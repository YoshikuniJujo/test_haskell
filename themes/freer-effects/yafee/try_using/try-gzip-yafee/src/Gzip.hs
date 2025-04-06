{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gzip where

import Foreign.C.Types
import Data.Bits
import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS

import Crc

import ByteStringNum

ids0 :: BS.ByteString
ids0 = "\x1f\x8b"

data GzipHeaderRaw = GzipHeaderRaw {
	gzipHeaderRawCompressionMethod :: CompressionMethod,
	gzipHeaderRawFlags :: FlagsRaw,
	gzipHeaderRawModificationTime :: CTime, -- Word32,
--	gzipHeaderRawModificationTime :: Word32,
	gzipHeaderRawExtraFlags :: Word8,
	gzipHeaderRawOperatingSystem :: OS,
	gzipHeaderRawFileName :: Maybe BS.ByteString }
	deriving Show

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime, -- Word32,
	gzipHeaderExtraFlags :: Word8,
	gzipHeaderOperatingSystem :: OS,
	gzipHeaderFileName :: Maybe BS.ByteString }
	deriving Show

gzipHeaderFromRaw :: GzipHeaderRaw -> GzipHeader
gzipHeaderFromRaw GzipHeaderRaw {
	gzipHeaderRawCompressionMethod = cm,
	gzipHeaderRawFlags = fs,
	gzipHeaderRawModificationTime = ct,
	gzipHeaderRawExtraFlags = efs,
	gzipHeaderRawOperatingSystem = os,
	gzipHeaderRawFileName = fn } = GzipHeader {
	gzipHeaderCompressionMethod = cm,
	gzipHeaderFlags = flagsFromRaw fs,
	gzipHeaderModificationTime = ct,
	gzipHeaderExtraFlags = efs,
	gzipHeaderOperatingSystem = os,
	gzipHeaderFileName = fn }

gzipHeaderToRaw :: GzipHeader -> GzipHeaderRaw
gzipHeaderToRaw GzipHeader {
	gzipHeaderCompressionMethod = cm,
	gzipHeaderFlags = fs,
	gzipHeaderModificationTime = ct,
	gzipHeaderExtraFlags = efs,
	gzipHeaderOperatingSystem = os,
	gzipHeaderFileName = fn
	} = GzipHeaderRaw {
	gzipHeaderRawCompressionMethod = cm,
	gzipHeaderRawFlags = flagsToRaw (isJust fn) fs,
	gzipHeaderRawModificationTime = ct,
	gzipHeaderRawExtraFlags = efs,
	gzipHeaderRawOperatingSystem = os,
	gzipHeaderRawFileName = fn }


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

readFlags :: Word8 -> Maybe FlagsRaw
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just FlagsRaw {
		flagsRawText = w `testBit` 0,
		flagsRawHcrc = w `testBit` 1,
		flagsRawExtra = w `testBit` 2,
		flagsRawName = w `testBit` 3,
		flagsRawComment = w `testBit` 4 }

encodeFlags :: FlagsRaw -> Word8
encodeFlags fs = foldl setBit 0 xs
	where
	xs = map fst . filter snd . zip [0 ..] $ ($ fs) <$>
		[flagsRawText, flagsRawHcrc, flagsRawExtra, flagsRawName, flagsRawComment]

data FlagsRaw = FlagsRaw {
	flagsRawText :: Bool,
	flagsRawHcrc :: Bool,
	flagsRawExtra :: Bool,
	flagsRawName :: Bool,
	flagsRawComment :: Bool }
	deriving Show

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool,
	flagsExtra :: Bool,
	flagsComment :: Bool }
	deriving Show

flagsFromRaw FlagsRaw {
	flagsRawText = t,
	flagsRawHcrc = h,
	flagsRawExtra = e,
	flagsRawComment = c } = Flags {
	flagsText = t,
	flagsHcrc = h,
	flagsExtra = e,
	flagsComment = c }

flagsToRaw fn Flags {
	flagsText = t,
	flagsHcrc = h,
	flagsExtra = e,
	flagsComment = c
	} = FlagsRaw {
	flagsRawText = t,
	flagsRawHcrc = h,
	flagsRawExtra = e,
	flagsRawName = fn,
	flagsRawComment = c }

encodeGzipHeader :: GzipHeaderRaw -> BS.ByteString
encodeGzipHeader hdr = ids0 `BS.append`
	(unCompressionMethod (gzipHeaderRawCompressionMethod hdr) `BS.cons`
		encodeFlags (gzipHeaderRawFlags hdr) `BS.cons`
		numToBs (cTimeToWord32 $ gzipHeaderRawModificationTime hdr)) `BS.append`
	(gzipHeaderRawExtraFlags hdr `BS.cons`
		unOS (gzipHeaderRawOperatingSystem hdr) `BS.cons` "") `BS.append`
		maybe "" (`BS.snoc` 0) (gzipHeaderRawFileName hdr)

sampleGzipHeader :: GzipHeader
sampleGzipHeader = GzipHeader {
	gzipHeaderCompressionMethod = CompressionMethodDeflate,
	gzipHeaderFlags = Flags {
		flagsText = False,
		flagsHcrc = False,
		flagsExtra = False,
		flagsComment = False },
	gzipHeaderModificationTime = 1743055415,
	gzipHeaderExtraFlags = 0,
	gzipHeaderOperatingSystem = OSUnix,
	gzipHeaderFileName = Just "abcd.txt" }

crc' :: BS.ByteString -> BS.ByteString
crc' = numToBs . crc

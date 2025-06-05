{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Data.Zlib where

import Data.Bits
import Data.Word

data Header = Header {
	headerCompressionMethod :: CompressionMethod,
	headerCompressionInfo :: Word8,
	headerFDict :: Bool,
	headerFLevel :: Word8
	}
	deriving Show

newtype CompressionMethod = CompressionMethod Word8

instance Show CompressionMethod where
	show (CompressionMethod 8) = "CompressionMethodDeflate"
	show (CompressionMethod cm) = "(CompressionMethod " ++ show cm ++ ")"

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 8

readHeader :: Word8 -> Word8 -> Maybe Header
readHeader cmf@(fromIntegral -> cmf') flg@(fromIntegral -> flg')
	| ((cmf' :: Word16) `shiftL` 8 + flg') `mod` 31 == 0 = Just Header {
		headerCompressionMethod = CompressionMethod $ cmf .&. 0x0f,
		headerCompressionInfo = cmf `shiftR` 4,
		headerFDict = testBit flg 5,
		headerFLevel = flg `shiftR` 6 }
	| otherwise = Nothing

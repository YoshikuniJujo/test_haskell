{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header where

import Data.Bits
import Data.Word

data Header = Header {
	headerWidth :: Word32,
	headerHeight :: Word32,
	headerBitDepth :: Word8,
	headerColorType :: ColorType,
	headerCompressionMethod :: CompressionMethod,
	headerFilterMethod :: FilterMethod,
	headerInterlaceMethod :: InterlaceMethod }
	deriving Show

header0 = Header {
	headerWidth = 0,
	headerHeight = 0,
	headerBitDepth = 0,
	headerColorType = ColorType 0,
	headerCompressionMethod = CompressionMethod 0,
	headerFilterMethod = FilterMethod 0,
	headerInterlaceMethod = InterlaceMethod 0 }

newtype ColorType = ColorType Word8 deriving (Eq, Bits)

pattern ColorTypePalletUsed, ColorTypeColorUsed, ColorTypeAlphaChannelUsed ::
	ColorType
pattern ColorTypePalletUsed = ColorType 1
pattern ColorTypeColorUsed = ColorType 2
pattern ColorTypeAlphaChannelUsed = ColorType 4

pattern ColorTypeGrayscale, ColorTypePallete, ColorTypeColorAlpha :: ColorType
pattern ColorTypeGrayscale = ColorType 0
pattern ColorTypeColor = ColorType 2
pattern ColorTypePallete = ColorType 3
pattern ColorTypeAlpha = ColorType 4
pattern ColorTypeColorAlpha = ColorType 6

sampleNum :: ColorType -> Int
sampleNum ColorTypeGrayscale = 1
sampleNum ColorTypeColorUsed = 3
sampleNum ColorTypePallete = 1
sampleNum ColorTypeAlpha = 2
sampleNum ColorTypeColorAlpha = 4
sampleNum _ = error "not allowed color type"

instance Show ColorType where
	show ColorTypePalletUsed  = "ColorTypePaletteUsed"
	show ColorTypeColorUsed = "ColorTypeColorUsed"
	show ColorTypeAlphaChannelUsed = "ColorTypeAlphaChannelUsed"
	show (ColorType n) = "(ColorType " ++ show n ++ ")"

newtype CompressionMethod = CompressionMethod Word8 deriving (Eq, Bits)

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 0

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show (CompressionMethod n) = "(CompressionMethod " ++ show n ++ ")"

newtype FilterMethod = FilterMethod Word8 deriving (Eq, Bits)

pattern FilterMethodDefaultFilter :: FilterMethod
pattern FilterMethodDefaultFilter = FilterMethod 0

instance Show FilterMethod where
	show FilterMethodDefaultFilter = "FilterMethodDefaultFilter"
	show (FilterMethod n) = "(FilterMethod " ++ show n ++ ")"

newtype InterlaceMethod = InterlaceMethod Word8 deriving (Eq, Bits)

pattern InterlaceMethodNon, InterlaceMethodAdam7 :: InterlaceMethod
pattern InterlaceMethodNon = InterlaceMethod 0
pattern InterlaceMethodAdam7 = InterlaceMethod 1

instance Show InterlaceMethod where
	show InterlaceMethodNon = "InterlaceMethodNon"
	show InterlaceMethodAdam7 = "InterlaceMethodAdam7"
	show (InterlaceMethod n) = "(InterlaceMethod " ++ show n ++ ")"

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header (

	-- * HEADER

	Header(..), encode, sampleNum,

	-- * COLOR TYPE

	ColorType,

	pattern ColorTypePaletteUsed,
	pattern ColorTypeColorUsed, pattern ColorTypeAlphaChannelUsed,

	pattern ColorTypeGrayscale, pattern ColorTypeColor,
	pattern ColorTypePalette,
	pattern ColorTypeAlpha, pattern ColorTypeColorAlpha,

	-- * COMPRESSION METHOD

	CompressionMethod, pattern CompressionMethodDeflate,

	-- * FILTER METHOD

	FilterMethod, pattern FilterMethodDefaultFilter,

	-- * INTERLACE METHOD

	InterlaceMethod,
	pattern InterlaceMethodNon, pattern InterlaceMethodAdam7

	) where


import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

-- HEADER

data Header = Header {
	width :: Word32, height :: Word32,
	bitDepth :: Word8, colorType :: ColorType,
	compressionMethod :: CompressionMethod,
	filterMethod :: FilterMethod, interlaceMethod :: InterlaceMethod }
	deriving Show

encode :: Header -> BS.ByteString
encode Header {
	width = wdt, height = hgt, bitDepth = bd, colorType = ColorType ct,
	compressionMethod = CompressionMethod cm,
	filterMethod = FilterMethod fm, interlaceMethod = InterlaceMethod im } =
	BS.fromBitsBE' wdt <> BS.fromBitsBE' hgt <> BS.pack [bd, ct, cm, fm, im]

sampleNum :: Integral n => Header -> n
sampleNum = colorTypeSampleNum . colorType

-- COLOR TYPE

newtype ColorType = ColorType Word8 deriving (Eq, Bits)

pattern ColorTypePaletteUsed,
	ColorTypeColorUsed, ColorTypeAlphaChannelUsed :: ColorType
pattern ColorTypePaletteUsed = ColorType 1
pattern ColorTypeColorUsed = ColorType 2
pattern ColorTypeAlphaChannelUsed = ColorType 4

pattern ColorTypeGrayscale, ColorTypePalette,
	ColorTypeColorAlpha, ColorTypeColor, ColorTypeAlpha :: ColorType
pattern ColorTypeGrayscale = ColorType 0
pattern ColorTypeColor = ColorType 2
pattern ColorTypePalette = ColorType 3
pattern ColorTypeAlpha = ColorType 4
pattern ColorTypeColorAlpha = ColorType 6

instance Show ColorType where
	show ColorTypePaletteUsed  = "ColorTypePaletteUsed"
	show ColorTypeColorUsed = "ColorTypeColorUsed"
	show ColorTypeAlphaChannelUsed = "ColorTypeAlphaChannelUsed"
	show (ColorType n) = "(ColorType " ++ show n ++ ")"

colorTypeSampleNum :: Num n => ColorType -> n
colorTypeSampleNum ColorTypeGrayscale = 1
colorTypeSampleNum ColorTypeColorUsed = 3
colorTypeSampleNum ColorTypePalette = 1
colorTypeSampleNum ColorTypeAlpha = 2
colorTypeSampleNum ColorTypeColorAlpha = 4
colorTypeSampleNum _ = error "not allowed color type"

-- COMPRESSION METHOD

newtype CompressionMethod = CompressionMethod Word8 deriving (Eq, Bits)

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 0

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show (CompressionMethod n) = "(CompressionMethod " ++ show n ++ ")"

-- FILTER METHOD

newtype FilterMethod = FilterMethod Word8 deriving (Eq, Bits)

pattern FilterMethodDefaultFilter :: FilterMethod
pattern FilterMethodDefaultFilter = FilterMethod 0

instance Show FilterMethod where
	show FilterMethodDefaultFilter = "FilterMethodDefaultFilter"
	show (FilterMethod n) = "(FilterMethod " ++ show n ++ ")"

-- INTERLACE METHOD

newtype InterlaceMethod = InterlaceMethod Word8 deriving (Eq, Bits)

pattern InterlaceMethodNon, InterlaceMethodAdam7 :: InterlaceMethod
pattern InterlaceMethodNon = InterlaceMethod 0
pattern InterlaceMethodAdam7 = InterlaceMethod 1

instance Show InterlaceMethod where
	show InterlaceMethodNon = "InterlaceMethodNon"
	show InterlaceMethodAdam7 = "InterlaceMethodAdam7"
	show (InterlaceMethod n) = "(InterlaceMethod " ++ show n ++ ")"

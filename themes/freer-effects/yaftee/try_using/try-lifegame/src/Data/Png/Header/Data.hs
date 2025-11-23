{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header.Data (

	-- * HEADER

	Header(..), header0, encodeHeader,

	-- * COLOR TYPE

	ColorType,
	pattern ColorTypeColor,
	pattern ColorTypeColorUsed,
	pattern ColorTypePalette,
	pattern ColorTypeAlpha,
	pattern ColorTypeGrayscale,
	pattern ColorTypeColorAlpha,
	pattern ColorTypePaletteUsed,
	pattern ColorTypeAlphaChannelUsed,

	-- * COMPRESSION METHOD

	CompressionMethod,
	pattern CompressionMethodDeflate,

	-- * FILTER METHOD

	FilterMethod,
	pattern FilterMethodDefaultFilter,

	-- * INTERLACE METHOD

	InterlaceMethod,
	pattern InterlaceMethodNon,
	pattern InterlaceMethodAdam7,

	) where


import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

data Header = Header {
	headerWidth :: Word32,
	headerHeight :: Word32,
	headerBitDepth :: Word8,
	headerColorType :: ColorType,
	headerCompressionMethod :: CompressionMethod,
	headerFilterMethod :: FilterMethod,
	headerInterlaceMethod :: InterlaceMethod }
	deriving Show

encodeHeader :: Header -> BS.ByteString
encodeHeader Header {
	headerWidth = wdt, headerHeight = hgt,
	headerBitDepth = bd, headerColorType = ColorType ct,
	headerCompressionMethod = CompressionMethod cm,
	headerFilterMethod = FilterMethod fm,
	headerInterlaceMethod = InterlaceMethod im } =
	BS.fromBitsBE' wdt <> BS.fromBitsBE' hgt <> BS.pack [bd, ct, cm, fm, im]

header0 :: Header
header0 = Header {
	headerWidth = 0,
	headerHeight = 0,
	headerBitDepth = 0,
	headerColorType = ColorType 0,
	headerCompressionMethod = CompressionMethod 0,
	headerFilterMethod = FilterMethod 0,
	headerInterlaceMethod = InterlaceMethod 0 }

newtype ColorType = ColorType Word8 deriving (Eq, Bits)

pattern ColorTypePaletteUsed, ColorTypeColorUsed, ColorTypeAlphaChannelUsed ::
	ColorType
pattern ColorTypePaletteUsed = ColorType 1
pattern ColorTypeColorUsed = ColorType 2
pattern ColorTypeAlphaChannelUsed = ColorType 4

pattern ColorTypeGrayscale, ColorTypePalette, ColorTypeColorAlpha,
	ColorTypeColor, ColorTypeAlpha :: ColorType
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

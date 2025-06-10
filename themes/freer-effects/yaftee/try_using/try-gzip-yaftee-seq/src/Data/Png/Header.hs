{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header where

import Data.Bits
import Data.Word
import Data.Int

data Header = Header {
	headerWidth :: Word32,
	headerHeight :: Word32,
	headerBitDepth :: Word8,
	headerColorType :: ColorType,
	headerCompressionMethod :: CompressionMethod,
	headerFilterMethod :: FilterMethod,
	headerInterlaceMethod :: InterlaceMethod }
	deriving Show

headerToRows :: Header -> [Int]
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodNon } =
	replicate (fromIntegral $ headerHeight h) (headerToBpp h * fromIntegral (headerWidth h))
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } = map (* headerToBpp h)
	$ interlacePixelNums
		(fromIntegral (headerWidth h))
		(fromIntegral $ headerHeight h)

interlacePixelNums :: Int -> Int -> [Int]
interlacePixelNums w h =
	replicate (h `div'` 8) (w `div'` 8) ++
	replicate (h `div'` 8) (w `div'` 4 `div` 2) ++
	replicate (h `div'` 4 `div` 2) (w `div'` 4) ++
	replicate (h `div'` 4) (w `div'` 2 `div` 2) ++
	replicate (h `div'` 2 `div` 2) (w `div'` 2) ++
	replicate (h `div'` 2) (w `div` 2) ++
	replicate (h `div` 2) w ++ [0]

m `div'`n = (m - 1) `div` n + 1

headerToBpp :: Integral n => Header -> n
headerToBpp hdr =
	(fromIntegral (headerBitDepth hdr) *
		sampleNum (headerColorType hdr) - 1) `div` 8 + 1

headerToRowBytes hdr =
	(fromIntegral (headerWidth hdr) * fromIntegral (headerBitDepth hdr) *
		sampleNum (headerColorType hdr) - 1) `div` 8 + 1

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

sampleNum :: Num n => ColorType -> n
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

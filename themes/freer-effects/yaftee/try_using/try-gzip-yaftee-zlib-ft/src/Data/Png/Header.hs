{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header where

import Data.Bits
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.Color
import Data.Adam7 qualified as Adam7

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

headerToRows :: Header -> [Int]
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodNon } =
	replicate (fromIntegral $ headerHeight h)
		((fromIntegral (headerWidth h) * fromIntegral (headerBitDepth h)) `div'` 8 * sampleNum' h)
		-- (headerToBpp h * fromIntegral (headerWidth h))
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	map ((* sampleNum' h) . (`div'` 8) . (* fromIntegral (headerBitDepth h)))
--	map (* headerToBpp h)
		$ interlacePixelNums
			(fromIntegral (headerWidth h))
			(fromIntegral $ headerHeight h)

headerToRows' :: Header -> Word32 -> Word32 -> [Int]
headerToRows' h@Header { headerInterlaceMethod = InterlaceMethodNon } wdt hgt =
	replicate (fromIntegral hgt)
		((fromIntegral wdt * fromIntegral (headerBitDepth h)) `div'` 8 * sampleNum' h)
headerToRows' h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } wdt hgt =
	map ((* sampleNum' h) . (`div'` 8) . (* fromIntegral (headerBitDepth h)))
--	map (* headerToBpp h)
		$ interlacePixelNums
			(fromIntegral wdt)
			(fromIntegral hgt)

headerToHeights :: Header -> [Int]
headerToHeights h@Header { headerInterlaceMethod = InterlaceMethodNon } =
	[fromIntegral $ headerHeight h]
headerToHeights h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	adam7heights . fromIntegral $ headerHeight h

headerToSizes :: Header -> [(Int, Int)]
headerToSizes h@Header { headerInterlaceMethod = InterlaceMethodNon } =
	[(fromIntegral $ headerWidth h, fromIntegral $ headerHeight h)]
headerToSizes h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	adam7Sizes	(fromIntegral $ headerWidth h)
			(fromIntegral $ headerHeight h)

adam7heights :: Int -> [Int]
adam7heights h = [
	h `div'` 8, h `div'` 8,
	h `div'` 4 `div` 2, h `div'` 4,
	h `div'` 2 `div` 2, h `div'` 2,
	h `div` 2 ]

adam7Sizes :: Int -> Int -> [(Int, Int)]
adam7Sizes w h = [
	(w `div'` 8, h `div'` 8),
	(w `div'` 4 `div` 2, h `div'` 8),
	(w `div'` 4, h `div'` 4 `div` 2),
	(w `div'` 2 `div` 2, h `div'` 4),
	(w `div'` 2, h `div'` 2 `div` 2),
	(w `div` 2, h `div'` 2), (w, h `div` 2) ]

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

sampleNum' = sampleNum . headerColorType

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

word8ListToRgbaList :: RealFrac d => Header -> [Word8] -> [Rgba d]
word8ListToRgbaList hdr@Header { headerBitDepth = bd, headerColorType = ct } =
	L.unfoldr (word8ListToRgba1 hdr)

word8ListToRgba1 :: RealFrac d => Header -> [Word8] -> Maybe (Rgba d, [Word8])
word8ListToRgba1 Header { headerBitDepth = bd, headerColorType = ct } = \case
	[] -> Nothing
	ws -> case (bd, ct) of
		(8, ColorTypeColorUsed) -> case ws of
			r : g : b : ws' -> Just (RgbaWord8 r g b 0xff, ws')
			_ -> Nothing
		(8, ColorType 6) -> case ws of
			r : g : b : a : ws' -> Just (RgbaWord8 r g b a, ws')
		(16, ColorTypeColorUsed) -> case ws of
			r1 : r0 : g1 : g0 : b1 : b0 : ws' -> Just (
				RgbaWord16
					(fromIntegral r1 `shiftL` 8 .|. fromIntegral r0)
					(fromIntegral g1 `shiftL` 8 .|. fromIntegral g1)
					(fromIntegral b1 `shiftL` 8 .|. fromIntegral b1)
					0xffff,
				ws' )
		(16, ColorType 6) -> case ws of
			r1 : r0 : g1 : g0 : b1 : b0 : a1 : a0 : ws' -> Just (
				RgbaWord16
					(fromIntegral r1 `shiftL` 8 .|. fromIntegral r0)
					(fromIntegral g1 `shiftL` 8 .|. fromIntegral g1)
					(fromIntegral b1 `shiftL` 8 .|. fromIntegral b1)
					(fromIntegral a1 `shiftL` 8 .|. fromIntegral a1),
				ws' )
		_ -> error "yet"

rgbaListToWord8List :: RealFrac d => Header -> [Rgba d] -> [Word8]
rgbaListToWord8List hdr = concatMap $ rgbaToWord8List hdr

rgbaToWord8List :: RealFrac d => Header -> Rgba d -> [Word8]
rgbaToWord8List Header { headerBitDepth = bd, headerColorType = ct } =
	case (bd, ct) of
		(8, ColorTypeColorUsed) -> \(RgbaWord8 r g b _) -> [r, g, b]
		(8, ColorType 6) -> \(RgbaWord8 r g b a) -> [r, g, b, a]
		(16, ColorTypeColorUsed) -> \(RgbaWord16 r g b _) -> [
			fromIntegral $ r `shiftR` 8, fromIntegral r,
			fromIntegral $ g `shiftR` 8, fromIntegral g,
			fromIntegral $ b `shiftR` 8, fromIntegral b ]
		(16, ColorType 6) -> \(RgbaWord16 r g b a) -> [
			fromIntegral $ r `shiftR` 8, fromIntegral r,
			fromIntegral $ g `shiftR` 8, fromIntegral g,
			fromIntegral $ b `shiftR` 8, fromIntegral b,
			fromIntegral $ a `shiftR` 8, fromIntegral a ]
		_ -> error "yet"

headerToPoss :: Header -> [(Int, Int)]
headerToPoss hdr@Header { headerInterlaceMethod = InterlaceMethodNon } =
	[ (fromIntegral x, fromIntegral y) |
		y <- [0 .. headerHeight hdr - 1], x <- [0 .. headerWidth hdr - 1] ]
headerToPoss hdr@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	concat $ Adam7.poss
		(fromIntegral $ headerWidth hdr) (fromIntegral $ headerHeight hdr)

headerToPoss' :: Header -> [[(Int, Int)]]
headerToPoss' hdr@Header { headerInterlaceMethod = InterlaceMethodNon } =
	(\y -> (, y) <$> [0 .. fromIntegral $ headerWidth hdr - 1]) <$> [0 .. fromIntegral $ headerHeight hdr - 1]
headerToPoss' hdr@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	Adam7.poss
		(fromIntegral $ headerWidth hdr) (fromIntegral $ headerHeight hdr)

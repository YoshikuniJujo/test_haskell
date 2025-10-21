{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header (

	headerToBpp, sampleNum',

	headerToRowBytes, headerToRows, headerToRows',

	headerToPoss, headerToPoss',

	rgbaListToWord8List, word8ListToRgbaList,
	calcSizes, calcPoss, calcPoss',

	) where


import Data.Bits
import Data.List qualified as L
import Data.Word
import Data.Color
import Data.Adam7 qualified as Adam7

import Tools

import Data.Png.Header.Data

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
headerToRows _ = error "bad"

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
headerToRows' _ _ _ = error "bad"

sampleNum' :: Header -> Int
sampleNum' = sampleNum . headerColorType

calcSizes :: Header -> Word32 -> Word32 -> [(Int, Int)]
calcSizes Header { headerInterlaceMethod = InterlaceMethodNon } w h =
	[(fromIntegral w, fromIntegral h)]
calcSizes Header { headerInterlaceMethod = InterlaceMethodAdam7 } w h =
	adam7Sizes	(fromIntegral w)
			(fromIntegral h)
calcSizes _ _ _ = error "bad"

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

headerToBpp :: Integral n => Header -> n
headerToBpp hdr =
	(fromIntegral (headerBitDepth hdr) *
		sampleNum (headerColorType hdr) - 1) `div` 8 + 1

headerToRowBytes :: Integral n => Header -> n
headerToRowBytes hdr =
	(fromIntegral (headerWidth hdr) * fromIntegral (headerBitDepth hdr) *
		sampleNum (headerColorType hdr) - 1) `div` 8 + 1

sampleNum :: Num n => ColorType -> n
sampleNum ColorTypeGrayscale = 1
sampleNum ColorTypeColorUsed = 3
sampleNum ColorTypePalette = 1
sampleNum ColorTypeAlpha = 2
sampleNum ColorTypeColorAlpha = 4
sampleNum _ = error "not allowed color type"

word8ListToRgbaList :: RealFrac d => Header -> [Word8] -> [Rgba d]
word8ListToRgbaList hdr = L.unfoldr (word8ListToRgba1 hdr)

word8ListToRgba1 :: RealFrac d => Header -> [Word8] -> Maybe (Rgba d, [Word8])
word8ListToRgba1 Header { headerBitDepth = bd, headerColorType = ct } = \case
	[] -> Nothing
	ws -> case (bd, ct) of
		(8, ColorTypeColorUsed) -> case ws of
			r : g : b : ws' -> Just (RgbaWord8 r g b 0xff, ws')
			_ -> Nothing
		(8, ColorTypeColorAlpha) -> case ws of
			r : g : b : a : ws' -> Just (RgbaWord8 r g b a, ws')
			_ -> error "bad"
		(16, ColorTypeColorUsed) -> case ws of
			r1 : r0 : g1 : g0 : b1 : b0 : ws' -> Just (
				RgbaWord16
					(fromIntegral r1 `shiftL` 8 .|. fromIntegral r0)
					(fromIntegral g1 `shiftL` 8 .|. fromIntegral g0)
					(fromIntegral b1 `shiftL` 8 .|. fromIntegral b0)
					0xffff,
				ws' )
			_ -> error "bad"
		(16, ColorTypeColorAlpha) -> case ws of
			r1 : r0 : g1 : g0 : b1 : b0 : a1 : a0 : ws' -> Just (
				RgbaWord16
					(fromIntegral r1 `shiftL` 8 .|. fromIntegral r0)
					(fromIntegral g1 `shiftL` 8 .|. fromIntegral g0)
					(fromIntegral b1 `shiftL` 8 .|. fromIntegral b0)
					(fromIntegral a1 `shiftL` 8 .|. fromIntegral a0),
				ws' )
			_ -> error "bad"
		_ -> error "yet"

rgbaListToWord8List :: RealFrac d => Header -> [Rgba d] -> [Word8]
rgbaListToWord8List hdr = concatMap $ rgbaToWord8List hdr

rgbaToWord8List :: RealFrac d => Header -> Rgba d -> [Word8]
rgbaToWord8List Header { headerBitDepth = bd, headerColorType = ct } =
	case (bd, ct) of
		(8, ColorTypeColorUsed) -> \(RgbaWord8 r g b _) -> [r, g, b]
		(8, ColorTypeColorAlpha) -> \(RgbaWord8 r g b a) -> [r, g, b, a]
		(16, ColorTypeColorUsed) -> \(RgbaWord16 r g b _) -> [
			fromIntegral $ r `shiftR` 8, fromIntegral r,
			fromIntegral $ g `shiftR` 8, fromIntegral g,
			fromIntegral $ b `shiftR` 8, fromIntegral b ]
		(16, ColorTypeColorAlpha) -> \(RgbaWord16 r g b a) -> [
			fromIntegral $ r `shiftR` 8, fromIntegral r,
			fromIntegral $ g `shiftR` 8, fromIntegral g,
			fromIntegral $ b `shiftR` 8, fromIntegral b,
			fromIntegral $ a `shiftR` 8, fromIntegral a ]
		_ -> error "yet"

headerToPoss :: Header -> [(Int, Int)]
headerToPoss hdr = calcPoss hdr (headerWidth hdr) (headerHeight hdr)

calcPoss :: Header -> Word32 -> Word32 -> [(Int, Int)]
calcPoss Header { headerInterlaceMethod = InterlaceMethodNon } w h =
	[ (fromIntegral x, fromIntegral y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
calcPoss Header { headerInterlaceMethod = InterlaceMethodAdam7 } w h =
	concat $ Adam7.poss (fromIntegral w) (fromIntegral h)
calcPoss _ _ _ = error "bad"

headerToPoss' :: Header -> [[(Int, Int)]]
headerToPoss' hdr = calcPoss' hdr (headerWidth hdr) (headerHeight hdr)

calcPoss' :: Header -> Word32 -> Word32 -> [[(Int, Int)]]
calcPoss' Header { headerInterlaceMethod = InterlaceMethodNon } w h =
	(\y -> (, y) <$> [0 .. fromIntegral $ w - 1]) <$> [0 .. fromIntegral $ h - 1]
calcPoss' Header { headerInterlaceMethod = InterlaceMethodAdam7 } w h =
	Adam7.poss (fromIntegral w) (fromIntegral h)
calcPoss' _ _ _ = error "bad"

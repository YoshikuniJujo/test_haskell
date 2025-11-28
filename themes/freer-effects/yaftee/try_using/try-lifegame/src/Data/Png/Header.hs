{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Header (

	headerToBpp,
	sampleNum',

	headerToRowBytes,

	headerToRows,

	calcSizes,
	calcPoss'


	) where


import Data.Bits
import Data.List qualified as L
import Data.Word
import Data.Color

import Tools

import Data.Png.Header.Data

headerToRows :: Header -> [Int]
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodNon } =
	replicate (fromIntegral $ headerHeight h)
		((fromIntegral (headerWidth h) * fromIntegral (headerBitDepth h)) `div'` 8 * sampleNum' h)
headerToRows h@Header { headerInterlaceMethod = InterlaceMethodAdam7 } =
	map ((* sampleNum' h) . (`div'` 8) . (* fromIntegral (headerBitDepth h)))
		$ interlacePixelNums
			(fromIntegral (headerWidth h))
			(fromIntegral $ headerHeight h)
headerToRows h = error $ "headerToRows: " ++ show h

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

headerToPoss :: Header -> [(Int, Int)]
headerToPoss hdr = calcPoss hdr (headerWidth hdr) (headerHeight hdr)

calcPoss :: Header -> Word32 -> Word32 -> [(Int, Int)]
calcPoss Header { headerInterlaceMethod = InterlaceMethodNon } w h =
	[ (fromIntegral x, fromIntegral y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
calcPoss Header { headerInterlaceMethod = InterlaceMethodAdam7 } _ _ =
	error "not implemented"
calcPoss _ _ _ = error "bad"

headerToPoss' :: Header -> [[(Int, Int)]]
headerToPoss' hdr = calcPoss' hdr (headerWidth hdr) (headerHeight hdr)

calcPoss' :: Header -> Word32 -> Word32 -> [[(Int, Int)]]
calcPoss' Header { headerInterlaceMethod = InterlaceMethodNon } w h =
	(\y -> (, y) <$> [0 .. fromIntegral $ w - 1]) <$> [0 .. fromIntegral $ h - 1]
calcPoss' Header { headerInterlaceMethod = InterlaceMethodAdam7 } _ _ =
	error "not implemented"
calcPoss' _ _ _ = error "bad"

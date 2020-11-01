{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.CairoPatternT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.PngSupport
import Graphics.Cairo.Values

checkFormat :: FilePath -> IO String
checkFormat fp = readPng fp >>= \case
	Right (ImageRGB8 _) -> pure "ImageRGB8"
	Right (ImageRGB16 _) -> pure "ImageRGB16"
	Right (ImageRGBA8 _) -> pure "ImageARGA8"
	Right (ImageRGBA16 _) -> pure "ImageARGA8"
	Left em -> pure $ "error: " ++ em
	_ -> pure "others"

{-
addAlpha :: FilePath -> FilePath -> IO ()
addAlpha src dst = readPng src >>= \case
	Left em -> error $ "error: " ++ em
	Right di -> writePng dst $ convertRGBA8 di
	-}

readPngImageRGB8 :: FilePath -> IO (Image PixelRGB8)
readPngImageRGB8 fp = readPng fp >>= \case
	Right (ImageRGB8 i) -> pure i
	_ -> error "no format"

addAlpha :: PixelRGB8 -> PixelRGBA8
addAlpha (PixelRGB8 r g b)
	| b_ > (r_ * 13 `div` 16) && b_ > (g_ * 13 `div` 16) = PixelRGBA8 r g b 0xff
--	| r_ > (b_ * 25 `div` 16) && g_ > (b_ * 25 `div` 16) = PixelRGBA8 r g b 0x00
--	| r > 0x7f && g > 0x7f && b > 0x9f = PixelRGBA8 r g b 0xff
	| r > 0x6f && g > 0x6f && b > 0x6f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

convertPngFile :: FilePath -> FilePath -> IO ()
convertPngFile src dst = writePng dst . pixelMap addAlpha =<< readPngImageRGB8 src

convertPngFileWith :: (PixelRGB8 -> PixelRGBA8) -> FilePath -> FilePath -> IO ()
convertPngFileWith aa src dst = writePng dst . pixelMap aa =<< readPngImageRGB8 src

addAlphaR :: PixelRGB8 -> PixelRGBA8
addAlphaR (PixelRGB8 r g b)
	| r_ > 0x6f && g_ > 0x6f && b_ > 0x6f = PixelRGBA8 r g b 0xff
	| 0.7 < g' / r' && g' / r' < 1.3 && 0.7 < b' / r' && b' / r' < 1.3 = PixelRGBA8 r g b 0xff
	| r_ > g_ && r_ > b_ = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]
	[r', g', b'] = fromIntegral <$> [r, g, b] :: [Double]

addAlphaG2 :: PixelRGB8 -> PixelRGBA8
addAlphaG2 (PixelRGB8 r g b)
	| g_ > (r_ * 10 `div` 9) && g_ > (b_ * 10 `div` 9) = PixelRGBA8 r g b 0x00
	| r_ < 0x7f && g_ > 0x67 && b_ < 0x7f = PixelRGBA8 r g b 0x00
--	| r < 0x8f && g > 0x8f && b < 0x7f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlphaG :: PixelRGB8 -> PixelRGBA8
addAlphaG (PixelRGB8 r g b)
	| g_ > (r_ * 8 `div` 7) && g_ > (b_ * 8 `div` 7) = PixelRGBA8 r g b 0x00
--	| r < 0x8f && g > 0x8f && b < 0x7f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlphaB :: PixelRGB8 -> PixelRGBA8
addAlphaB (PixelRGB8 r g b)
	| r_ < 0x8f && g_ < 0x8f && b_ < 0x8f = PixelRGBA8 r g b 0xff
	| b_ > r_ && b_ > g_ = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

convertPngFile2 :: FilePath -> FilePath -> IO ()
convertPngFile2 src dst = writePng dst . pixelMapWithPos 450 795
		(\x y p -> case () of
			_	| x < 50 -> PixelRGBA8 0 0 0 0
				| y < 140 -> addAlpha1 p
				| y < 330 -> addAlpha2 p
				| y < 650 -> addAlpha3 p
				| y < 700 -> addAlpha4 p
				| y < 795 -> addAlpha5 p
				| otherwise -> addAlpha6 p)
	=<< readPngImageRGB8 src

addAlpha1 :: PixelRGB8 -> PixelRGBA8
addAlpha1 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0x77 && g > 0x77 && b < 0x77 = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlpha2 :: PixelRGB8 -> PixelRGBA8
addAlpha2 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0xa7 && g > 0xa7 && b < 0x47 = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlpha3 :: PixelRGB8 -> PixelRGBA8
addAlpha3 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0x3f && g > 0x3f && b < 0x8f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlpha4 :: PixelRGB8 -> PixelRGBA8
addAlpha4 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0x6f && g > 0x6f && b < 0x3f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlpha5 :: PixelRGB8 -> PixelRGBA8
addAlpha5 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0x5f && g > 0x5f && b < 0x3f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

addAlpha6 :: PixelRGB8 -> PixelRGBA8
addAlpha6 (PixelRGB8 r g b)
--	| r_ > (b_ * 24 `div` 16) && g_ > (b_ * 24 `div` 16) = PixelRGBA8 r g b 0xff
	| r > 0x6f && g > 0x5f && b < 0x3f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff
	where
	[r_, g_, b_] = fromIntegral <$> [r, g, b] :: [Integer]

pixelMapWithPos :: (Pixel a, Pixel b) => Int -> Int -> (Int -> Int -> a -> b) -> Image a -> Image b
pixelMapWithPos w h f src = generateImage (\x y -> f x y $ pixelAt src x y) w h

pngOnPngScaled :: FilePath -> FilePath -> Double -> Double -> Double -> FilePath -> IO ()
pngOnPngScaled src1 src2 scl tx ty dst = do
	s1 <- cairoSurfaceCreateFromPng src1
	s2 <- cairoSurfaceCreateFromPng src2
	cr <- cairoCreate s1
	pt <- cairoPatternCreateForSurface s2
	cairoScale cr scl scl
	cairoTranslate cr tx ty
	cairoSetSource cr pt
	cairoPaint cr
	cairoSurfaceWriteToPng s1 dst
	pure ()

addBack :: Double -> Double -> Double -> FilePath -> FilePath -> IO ()
addBack r g b src dst = do
	sfc <- cairoImageSurfaceCreate cairoFormatArgb32 1280 640
	s <- cairoSurfaceCreateFromPng src
	cr <- cairoCreate sfc
	cairoSetSourceRgb cr r g b
	cairoPaint cr
	pt <- cairoPatternCreateForSurface s
	cairoSetSource cr pt
	cairoPaint cr
	cairoSurfaceWriteToPng sfc dst
	pure ()

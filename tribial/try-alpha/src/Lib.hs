{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.CairoPatternT
import Graphics.Cairo.PngSupport

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

pngOnPng :: FilePath -> FilePath -> FilePath -> IO ()
pngOnPng src1 src2 dst = do
	s1 <- cairoSurfaceCreateFromPng src1
	s2 <- cairoSurfaceCreateFromPng src2
	cr <- cairoCreate s1
	pt <- cairoPatternCreateForSurface s2
	cairoSetSource cr pt
	cairoPaint cr
	cairoSurfaceWriteToPng s1 dst
	pure ()

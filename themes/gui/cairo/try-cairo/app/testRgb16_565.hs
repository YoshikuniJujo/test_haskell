{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

import Paths_try_cairo
import Parts (checkPattern)

main :: IO ()
main = do
	putStrLn "*** TEST RGB 16 565 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatRgb16_565 256 256
	cr <- cairoCreate sfc0
	checkPattern cr 256 256
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageRgb16_565
		=<< readRgb16_565 =<< getDataFileName "HaskellLogoNoAlpha.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageRgb16_565 i -> writeRgb16_565 "HaskellLogoNoAlphaRotated16_565.png" i
		_ -> error "image format error"
	putStrLn "*** TEST RGB 16 565 END ***"

readRgb16_565 :: FilePath -> IO Rgb16_565
readRgb16_565 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGB8 i) -> pure $ juicyRGB8ToCairoRgb16_565 i
	_ -> error "image format error"

writeRgb16_565 :: FilePath -> Rgb16_565 -> IO ()
writeRgb16_565 fp = writePng fp . cairoRgb16_565ToJuicyRGB8

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
	putStrLn "*** TEST RGB 24 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatRgb24 256 256
	cr <- cairoCreate sfc0
	checkPattern cr 256 256
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageRgb24
		=<< readRgb24 =<< getDataFileName "HaskellLogoNoAlpha.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageRgb24 i -> writeRgb24 "HaskellLogoNoAlphaRotated.png" i
		_ -> error "image format error"
	putStrLn "*** TEST RGB 24 END ***"

readRgb24 :: FilePath -> IO Rgb24
readRgb24 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGB8 i) -> pure $ juicyRGB8ToCairoRgb24 i
	_ -> error "image format error"

writeRgb24 :: FilePath -> Rgb24 -> IO ()
writeRgb24 fp = writePng fp . cairoRgb24ToJuicyRGB8

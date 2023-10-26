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
	putStrLn "*** TEST RGB 30 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatRgb30 256 256
	cr <- cairoCreate sfc0
	checkPattern cr 256 256
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageRgb30
		=<< readRgb30 =<< getDataFileName "HaskellLogoNoAlpha16.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageRgb30 i -> writeRgb30 "HaskellLogoNoAlphaRotated30.png" i
		_ -> error "image format error"
	putStrLn "*** TEST RGB 30 END ***"

readRgb30 :: FilePath -> IO Rgb30
readRgb30 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGB16 i) -> pure $ juicyRGB16ToCairoRgb30 i
	_ -> error "image format error"

writeRgb30 :: FilePath -> Rgb30 -> IO ()
writeRgb30 fp = writePng fp . cairoRgb30ToJuicyRGB16

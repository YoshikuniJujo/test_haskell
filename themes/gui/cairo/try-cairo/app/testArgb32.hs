{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

import Paths_try_cairo

main :: IO ()
main = do
	putStrLn "*** TEST ARGB 32 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate cairoFormatArgb32 256 256
	cr <- cairoCreate sfc0
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageArgb32
		=<< readArgb32 =<< getDataFileName "HaskellLogo.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> writeArgb32 "HaskellLogoRotated.png" i
		_ -> error "image format error"
	putStrLn "*** TEST ARGB 32 END ***"

readArgb32 :: FilePath -> IO Argb32
readArgb32 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGBA8 i) -> pure $ juicyRGBA8ToCairoArgb32 i
	_ -> error "image format error"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

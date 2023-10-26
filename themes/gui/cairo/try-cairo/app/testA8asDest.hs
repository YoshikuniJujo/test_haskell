{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage.Internal
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces

import Paths_try_cairo
import Parts (readArgb32)

import Codec.Picture
import Data.JuicyCairo

main :: IO ()
main = do
	putStrLn "*** TEST ARGB 32 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatA8 256 256
	cr <- cairoCreate sfc0
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageArgb32
		=<< readArgb32 =<< getDataFileName "HaskellLogo.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageA8 i -> do
			print $ imageSize i -- writeArgb32 "HaskellLogoRotated.png" i
			writePng "HaskellLogoRotatedA8.png" $ cairoA8ToJuicyYA8 0x77 i
		_ -> error "image format error"
	putStrLn "*** TEST ARGB 32 END ***"

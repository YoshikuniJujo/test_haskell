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
	putStrLn "*** TEST A 1 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatA1 256 256
	cr <- cairoCreate sfc0
	sfc <- cairoImageSurfaceCreateForCairoImage . CairoImageArgb32
		=<< readArgb32 =<< getDataFileName "HaskellLogo.png"
	ptn <- cairoPatternCreateForSurface sfc
	cairoTranslate cr 128 ((256 - sqrt 2 * 128) / 2)
	cairoRotate cr (pi / 4)
	cairoSetSource cr ptn
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageA1 i -> do
			print $ imageSize i
			writePng "HaskellLogoRotatedA1RGBA.png" $ cairoA1ToJuicyRGBA8 0x00 0x77 0x00 i
		_ -> error "image format error"
	putStrLn "*** TEST A 1 END ***"

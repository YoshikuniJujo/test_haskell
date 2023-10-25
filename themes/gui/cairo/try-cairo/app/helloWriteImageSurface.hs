{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.CairoImage
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.Maybe
import Data.Color
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.Paths

main :: IO ()
main = do
	putStrLn "*** TEST ARGB 32 BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate cairoFormatArgb32 256 256
	cr <- cairoCreate sfc0

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> writeArgb32 "helloWriteImageSurface.png" i
		_ -> error "image format error"
	putStrLn "*** TEST ARGB 32 END ***"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleImages where

import Control.Monad.Primitive
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

twoRectangles :: PrimMonad m => m CairoImage
twoRectangles = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoImageSurfaceGetCairoImage sfc0

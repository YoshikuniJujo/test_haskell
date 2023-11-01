{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleImages (

	-- * TWO RECTANGLES

	twoRectangles, twoRectanglesPrim, twoRectanglesPrim',

	-- * TYPE AND ENUMS

	Image(..), Argb32, PixelArgb32, pattern CairoFormatArgb32,

	-- * SURFACE AND CAIRO CONTEXT

	cairoImageSurfaceCreate, CairoSurfaceImageT, RealWorld,
	cairoCreate, CairoT

	) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.Color
import Data.CairoContext
import Data.CairoImage.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

twoRectangles :: Argb32
twoRectangles = runST twoRectanglesPrim

twoRectanglesPrim :: PrimMonad m => m Argb32
twoRectanglesPrim = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0

	twoRectanglesPrim' sfc0 cr

twoRectanglesPrim' :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> m Argb32
twoRectanglesPrim' sfc0 cr = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoSurfaceFlush sfc0

	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"

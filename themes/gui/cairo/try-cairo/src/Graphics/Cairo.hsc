{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo (
	CairoT, CairoSurfaceT, CairoStatusT,
	cairoFill, cairoRectangle, cairoSetSourceRgba, cairoStroke, cairoSetLineWidth,
	cairoSetSourceRgb, cairoCreate, cairoFormatArgb32, cairoImageSurfaceCreate,
	cairoPaintWithAlpha, cairoPaint,

	cairoStatusSuccess, cairoStatusNoMemory, cairoStatusInvalidRestore, cairoFormatInvalid
	) where

#include <cairo.h>

import Graphics.Cairo.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Types
import Graphics.Cairo.Values

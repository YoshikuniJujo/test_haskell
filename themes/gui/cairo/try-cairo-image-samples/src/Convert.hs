{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Convert (

	-- * CAIRO IMAGE SURFACE PIXELS

	PixelArgb32,

	-- * COLORS

	Rgba, pattern RgbaWord8,

	-- * CONVERT

	argb32ToRgba

	) where

import Data.Maybe
import Data.Color
import Data.CairoImage

argb32ToRgba :: PixelArgb32 -> Rgba d
argb32ToRgba (PixelArgb32Premultiplied a r g b) =
	fromJust $ rgbaPremultipliedWord8 a r g b

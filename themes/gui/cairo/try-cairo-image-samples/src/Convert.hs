{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Convert (

	-- * CAIRO IMAGE SURFACE PIXELS

	PixelArgb32,

	-- * COLORS

	Rgba, pattern RgbaWord8,

	-- * CONVERT

	pixelArgb32ToRgba, rgbaToPixelArgb32

	) where

import Data.Maybe
import Data.Color
import Data.CairoImage

pixelArgb32ToRgba :: PixelArgb32 -> Rgba d
pixelArgb32ToRgba (PixelArgb32Premultiplied a r g b) =
	fromJust $ rgbaPremultipliedWord8 a r g b

rgbaToPixelArgb32 :: RealFrac d => Rgba d -> PixelArgb32
rgbaToPixelArgb32 (RgbaPremultipliedWord8 a r g b) =
	fromJust $ pixelArgb32Premultiplied a r g b

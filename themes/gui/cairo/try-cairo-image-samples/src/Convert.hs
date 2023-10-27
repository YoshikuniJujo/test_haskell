{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Convert where

import Data.Maybe
import Data.Color
import Data.CairoImage

argb32ToRgba :: PixelArgb32 -> Rgba d
argb32ToRgba (PixelArgb32Premultiplied a r g b) =
	fromJust $ rgbaPremultipliedWord8 a r g b

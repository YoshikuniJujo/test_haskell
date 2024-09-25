{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ConvertPixel (

	-- * CAIRO IMAGE SURFACE PIXELS

	PixelArgb32,

	-- * CONVERT

	pixelArgb32ToRgba, rgbaToPixelArgb32

	) where

import Data.Maybe
import Data.Color
import Data.CairoImage

pixelArgb32ToRgba :: PixelArgb32 -> Rgba d
pixelArgb32ToRgba (PixelArgb32Premultiplied a r g b) = fromJustWithErrorMsg (
		"pixelArgb32ToRgba: (a, r, g, b) = (" ++
		show a ++ ", " ++ show r ++ ", " ++
		show g ++ ", " ++ show b ++ ")" )
	$ rgbaPremultipliedWord8 r g b a

rgbaToPixelArgb32 :: RealFrac d => Rgba d -> PixelArgb32
rgbaToPixelArgb32 (RgbaPremultipliedWord8 a r g b) =
	fromJust $ pixelArgb32Premultiplied a r g b

fromJustWithErrorMsg :: String -> Maybe a -> a
fromJustWithErrorMsg msg = \case Nothing -> error msg; Just x -> x

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim
import Graphics.Pango.Rendering.Cairo

main :: IO ()
main = getArgs >>= \case
	f : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s
		pl <- pangoCairoCreateLayout cr

		fd <- pangoFontDescriptionNew
		case f of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ Family f
		print =<< pangoFontDescriptionGet @Family fd

		pangoLayoutSetFontDescription pl fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!" 40

		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"

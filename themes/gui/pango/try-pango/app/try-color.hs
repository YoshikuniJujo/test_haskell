{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.VerticalText
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 400 600
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Size 24
	fd' <- pangoFontDescriptionFreeze fd

	al <- pangoTextAttrListNew "Hello, world!\nこんにちは、世界!\n\x1f9a5"
	at1 <- pangoAttrNew $ ForegroundColor (maxBound `div` 10) (maxBound `div` 2) (maxBound `div` 20)
	at2 <- pangoAttrNew $ ForegroundAlpha 0
	at3 <- pangoAttrNew $ ForegroundAlpha 10
	pangoTextAttrListInsert al at1 0 15
	pangoTextAttrListInsert al at2 10 17
	pangoTextAttrListInsert al at3 17 19

	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al

	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "pngs/try-color.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

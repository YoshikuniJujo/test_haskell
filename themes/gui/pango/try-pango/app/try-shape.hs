{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.TextAttributes

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 400 600
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Size 24
	fd' <- pangoFontDescriptionFreeze fd

	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
--	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!\n\x1f9a5"
	al <- pangoTextAttrListNew "Hello, world!\nこんにちは、世界!\n\x1f9a5"
	at <- pangoAttrNew $ Shape
		(PangoRectangleFixed 5 3 16 16)
		(PangoRectangleFixed 0 0 35 35)
	pangoTextAttrListInsert al at 2 10
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al

	cairoMoveTo cr 16 16
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-shape.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

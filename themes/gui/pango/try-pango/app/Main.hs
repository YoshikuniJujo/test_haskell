{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types

import Control.Monad.Primitive
import Codec.Picture
import Data.Maybe

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Values
import Graphics.Pango.Rendering.Cairo

import Data.Color
import Data.CairoContext
import Data.CairoImage
import Data.JuicyCairo

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s

	helloWorld cr (0, 0.5, 0)
		"sans-serif"
		pangoStyleNormal pangoVariantNormal pangoWeightThin pangoStretchNormal
		(0, 0)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		pangoStyleNormal pangoVariantNormal pangoWeightUltraheavy pangoStretchNormal
		(0, 100)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		pangoStyleOblique pangoVariantNormal pangoWeightNormal pangoStretchUltraCondensed
		(0, 200)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		pangoStyleItalic pangoVariantSmallCaps pangoWeightNormal pangoStretchUltraExpanded
		(0, 300)

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "tmp.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

helloWorld :: CairoT RealWorld ->
	(CDouble, CDouble, CDouble) ->
	String -> PangoStyle -> PangoVariant -> PangoWeight -> PangoStretch ->
	(CDouble, CDouble) -> IO ()
helloWorld cr (r, g, b) ff stl vr wt strc (x, y) = do
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetFamily pfd ff
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoFontDescriptionSetStyle pfd stl
	pangoFontDescriptionSetVariant pfd vr
	pangoFontDescriptionSetWeight pfd wt
	pangoFontDescriptionSetStretch pfd strc

	pfd' <- pangoFontDescriptionFreeze pfd
	putStrLn $ pangoFontDescriptionToString pfd'
	putStrLn $ pangoFontDescriptionToFilename pfd'

	pangoLayoutSetFontDescription pl pfd'
	pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 40
	cairoSetSourceRgb cr . fromJust $ rgbDouble r g b
	cairoIdentityMatrix cr
	cairoTranslate cr x y
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

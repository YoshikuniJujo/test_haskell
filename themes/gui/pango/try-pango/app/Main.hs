{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Types
import Graphics.Pango.Values
import Lib

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

	void $ writeDynamicPng "tmp.png" =<< cairoImageSurfaceGetImage s
	pure ()

helloWorld :: CairoT RealWorld ->
	(Double, Double, Double) ->
	String -> PangoStyle -> PangoVariant -> PangoWeight -> PangoStretch ->
	(Double, Double) -> IO ()
helloWorld cr (r, g, b) ff stl vr wt strc (x, y) = do
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	let	pfdo = pangoFontDescriptionPrimToOld pfd
	pangoFontDescriptionSetFamily pfd ff
	pangoFontDescriptionSetSize pfdo (30 * pangoScale)
	pangoFontDescriptionSetStyle pfd stl
	pangoFontDescriptionSetVariant pfd vr
	pangoFontDescriptionSetWeight pfd wt
	pangoFontDescriptionSetStretch pfdo strc
	putStrLn =<< pangoFontDescriptionToString pfdo
	putStrLn =<< pangoFontDescriptionToFilename pfdo
	pangoLayoutSetFontDescription pl pfdo
	pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 40
	cairoSetSourceRgb cr r g b
	cairoIdentityMatrix cr
	cairoTranslate cr x y
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

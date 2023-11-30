{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types

import Control.Monad.Primitive
import Codec.Picture
import Data.Kind
import Data.Maybe
import Data.Fixed

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Data.Color
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 600 800
	cr <- cairoCreate s

	helloWorld cr (0, 0.5, 0)
		"sans-serif"
		PangoStyleNormal PangoVariantNormal PangoWeightThin PangoStretchNormal
		(0, 0)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		PangoStyleNormal PangoVariantNormal PangoWeightUltraheavy PangoStretchNormal
		(0, 100)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		PangoStyleOblique PangoVariantNormal PangoWeightNormal PangoStretchUltraCondensed
		(0, 200)
	helloWorld cr (0.3, 0.25, 0)
		"serif"
		PangoStyleItalic PangoVariantSmallCaps PangoWeightNormal PangoStretchUltraExpanded
		(0, 300)

	cairoIdentityMatrix cr
	cairoTranslate cr 50 50
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl "Hello"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-exe.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

helloWorld :: CairoT r RealWorld ->
	(CDouble, CDouble, CDouble) ->
	String -> PangoStyle -> PangoVariant -> PangoWeight -> PangoStretch ->
	(CDouble, CDouble) -> IO ()
helloWorld cr (r, g, b) ff stl vr wt strc (x, y) = do
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd $ Family ff
	{-
	pangoFontDescriptionSet pfd . Size . fromIntegral $ 30 * resolution @Type @PU undefined
	-}
	pangoFontDescriptionSet pfd stl
	pangoFontDescriptionSet pfd vr
	pangoFontDescriptionSet pfd wt
	pangoFontDescriptionSet pfd strc

	pfd' <- pangoFontDescriptionFreeze pfd
	print $ pangoFontDescriptionToString pfd'
	print $ pangoFontDescriptionToFilename pfd'

	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just pfd'
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは世界!"
	print . pangoLayoutInfo @CharacterCount =<< pangoLayoutFreeze pl
	cairoSetSourceRgb cr . fromJust $ rgbDouble r g b
	cairoIdentityMatrix cr
	cairoTranslate cr x y
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

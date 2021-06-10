{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.ST
import Data.Kind
import Data.Fixed
import Data.Angle
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Rendering.PangoContext
import Graphics.Pango.Basic.GlyphStorage.PangoMatrix
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Data.CairoImage
import Data.JuicyCairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 700 600
	cr <- cairoCreate s
	cairoScale cr 2.5 2
	cairoRotate cr (pi / 4)
	cairoTranslate cr 60 0
	cxt <- pangoCairoCreateContext cr

	cm <- pangoContextGet cxt
	print cm
	print $ pangoMatrixFromNullable cm

{-
	pangoContextSet cxt . pangoMatrixToNullable . Just
		. (`pangoMatrixRotatePure` Degree 45) $ PangoMatrix 2 0 0 2 0 0

	cm <- pangoContextGet cxt
	print cm
	print $ pangoMatrixFromNullable cm
-}

	pangoContextSet cxt PangoFontDescriptionNull

	pl <- pangoLayoutNew cxt

	pfd <- pangoFontDescriptionPrimNew
	pangoFontDescriptionSetSize pfd . fromIntegral $ 30 * resolution @Type @PU undefined
	pangoLayoutSetFontDescription pl =<< pangoFontDescriptionFreeze pfd

	pangoLayoutSet @T.Text pl "こんにちは世界!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
--	void $ writeDynamicPng "tmp2.png" =<< cairoImageSurfaceGetImage s
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-context.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

pangoMatrixRotatePure :: PangoMatrix -> Angle CDouble -> PangoMatrix
pangoMatrixRotatePure m a = runST do
	mp <- pangoMatrixThaw m
	pangoMatrixRotate mp a
	pangoMatrixFreeze mp

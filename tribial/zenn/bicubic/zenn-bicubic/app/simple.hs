{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 300 400
	cr <- cairoCreate s

	cairoSet cr $ LineWidth 8
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.8 0
	cairoMoveTo cr 20 20
	cairoLineTo cr 150 150
	cairoLineTo cr 20 280
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl "あいうえお\nfoobar"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "simple.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

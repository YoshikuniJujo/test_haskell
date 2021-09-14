{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import MakePng

import qualified Data.Text as T

main :: IO ()
main = pngWith "pngs/try-sloth.png" 362 256 \cr -> do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.8 0.5
	cairoPaint cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Size 36

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl . pangoFontDescriptionToNullable . Just
		=<< pangoFontDescriptionFreeze fd
	pangoLayoutSet @T.Text pl "\x01f9a5ナマケモノ"

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.05
	cairoMoveTo cr 24 32
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.0 0.7 0.0
	cairoMoveTo cr 24 96
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.4 0.2
	cairoMoveTo cr 24 160
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

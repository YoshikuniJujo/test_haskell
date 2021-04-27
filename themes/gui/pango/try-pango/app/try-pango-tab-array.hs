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
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.TabStops

import qualified Data.Text as T

main :: IO ()
main = do
	tad <- pangoTabArrayDoubleNew
	pangoTabArrayDoubleSetTab tad 3 100
	pangoTabArrayDoubleSetTab tad 5 250
	print $ unsafeTryTabArrayDoubleGetTabs tad

	tai <- pangoTabArrayIntNew
	pangoTabArrayIntSetTab tai 10 100
	print $ unsafeTryTabArrayIntGetTabs tai

	s <- cairoImageSurfaceCreate cairoFormatArgb32 600 400
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
	pangoCairoShowLayout cr pl

	pangoLayoutSet @T.Text pl "a\tb\tc\td\te\tf\tg"
	cairoMoveTo cr 0 50
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 70
	pangoLayoutSetTabs pl =<< pangoTabArrayDoubleFreeze tad
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 90
	pangoLayoutSetTabs pl =<< pangoTabArrayIntFreeze tai
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-tab-array.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

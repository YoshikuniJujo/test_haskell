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
	taf <- pangoTabArrayFixedNew
	pangoTabArrayFixedSetTab taf 3 100
	pangoTabArrayFixedSetTab taf 5 250
	taf' <- pangoTabArrayFixedFreeze taf
	print $ pangoTabArrayGetTabs taf'

	tai <- pangoTabArrayIntNew
	pangoTabArrayIntSetTab tai 10 100
	tai' <- pangoTabArrayIntFreeze tai
	print $ pangoTabArrayGetTabs tai'

	s <- cairoImageSurfaceCreate cairoFormatArgb32 600 400
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	pangoLayoutSet @T.Text pl "a\tb\tc\td\te\tf\tg"
	cairoMoveTo cr 0 60
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 150
	pangoLayoutSet pl . pangoTabArrayToNullable $ Just taf'
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 200
	pangoLayoutSet pl . pangoTabArrayToNullable $ Just tai'
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-tab-array.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

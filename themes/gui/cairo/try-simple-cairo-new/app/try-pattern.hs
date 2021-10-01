{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.CairoPatternT.Setting
import Graphics.Cairo.Drawing.Paths
import MakePng

main :: IO ()
main = pngWith "pngs/try-pattern.png" 768 768 \cr -> do

	cairoRectangle cr 8 8 176 176
	cairoClip cr
	pt <- cairoPatternCreateLinear 64 64 128 128
	print =<< cairoPatternGetFilter pt

	cairoPatternSet pt CairoExtendNone
	cairoPatternAddColorStopRgb pt 0.1 . fromJust $ rgbDouble 0 0.6 0
	cairoPatternAddColorStopRgb pt 0.9 . fromJust $ rgbDouble 0.6 0 0
	cairoSetSource cr pt
	cairoPaint cr

	cairoResetClip cr

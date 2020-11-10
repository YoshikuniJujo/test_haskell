{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Values

import Lib

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 700 500
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl pfd
	pangoLayoutSetWidth pl (150 * pangoScale)
	pangoLayoutSetText pl "こんにちは世界!" 100
	cairoMoveTo cr 100 100
	pangoCairoShowLayout cr pl
	void $ writeDynamicPng "tmp3.png" =<< cairoImageSurfaceGetImage s

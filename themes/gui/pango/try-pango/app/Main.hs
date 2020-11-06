{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Values
import Lib

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 100
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl pfd
	pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 40
	cairoSetSourceRgb cr 0 0.5 0
	pangoCairoShowLayout cr pl
	void $ writeDynamicPng "tmp.png" =<< cairoImageSurfaceGetImage s
	pure ()

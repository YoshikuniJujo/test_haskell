{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Values
import Lib

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s
	pc <- pangoCairoCreateContext cr
	pl <- pangoLayoutNew pc
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl pfd
	pangoLayoutSetText pl "こんにちは世界!" 30
	pangoCairoShowLayout cr pl
	void $ writeDynamicPng "tmp2.png" =<< cairoImageSurfaceGetImage s

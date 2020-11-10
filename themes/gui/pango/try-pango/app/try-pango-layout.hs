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
--	pangoFontDescriptionSetFamily pfd "sans-serif"
	pangoFontDescriptionSetFamily pfd "serif"
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl pfd
	pangoLayoutSetWidth pl (200 * pangoScale)
	pangoLayoutSetEllipsize pl pangoEllipsizeMiddle
--	pangoLayoutSetText pl "こんにちは世界!" 100
	pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 100
--	pangoLayoutSetText pl "Hello, world!\x2026\x22ef\nこんにちは世界!\x2026\x22ef" 100
--	pangoLayoutSetText pl "Hello, world!\x22ef\x2026\x22ef" 100
	cairoMoveTo cr 100 100
	pangoCairoShowLayout cr pl
	void $ writeDynamicPng "tmp3.png" =<< cairoImageSurfaceGetImage s

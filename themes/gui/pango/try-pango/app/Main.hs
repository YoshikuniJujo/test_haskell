{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.CairoImage
import Graphics.Cairo.Values

import Lib

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 100
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetText pl "Hello, world!" 15
	pangoCairoShowLayout cr pl
	writeDynamicPng "tmp.png" =<< cairoImageSurfaceGetImage s
	pure ()

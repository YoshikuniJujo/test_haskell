{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.ImageData.Text
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Trial.TryPango
import Trial.MakePng

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate sr
	drawFont cr 16 16 (Font "Hermit") "abcあいう"
	drawFont cr 16 64 (Font "sans") "abcあいう"
	drawFont cr 16 112 (Font "serif") "abcあいう"
	drawFont cr 16 160 (Font "soulcraft") "abcあいう"
	makePng sr "pngs/try-font.png"

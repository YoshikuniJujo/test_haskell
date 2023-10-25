{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.ImageData.Text
import Trial.TryPango
import Trial.MakePng

import Data.CairoImage.Internal

scale :: Double
scale = 16

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32
		(round $ 224 * scale) (round $ 104 * scale)
	cr <- cairoCreate sr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	cairoPaint cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoMoveTo cr (realToFrac $ 40 * scale) 0
	drawLayout cr . sampleTeX $ 64 * scale

	makePng sr "pngs/try-tex.png"

sampleTeX :: Double -> Layout
sampleTeX sz = Layout {
	layoutAttrs = defaultLayoutAttrs,
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "serif" sz) {
			textAttrsLetterSpacing = LetterSpacing $ - sz / 3 } "T",
		Text (textAttrsFromFont $ sampleFont "serif" sz) {
			textAttrsRise = Rise $ - sz / 4 } "E",
		Text (textAttrsFromFont $ sampleFont "serif" sz) {
			textAttrsLetterSpacing = LetterSpacing $ - sz / 2 } "X"
		] }

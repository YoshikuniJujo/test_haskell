{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.ImageData.Text
import Trial.TryPango
import Trial.MakePng

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 224 104
	cr <- cairoCreate sr

	cairoMoveTo cr 40 0
	drawLayout cr $ sampleTeX 64

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

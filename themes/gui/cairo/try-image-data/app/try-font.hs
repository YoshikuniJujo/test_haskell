{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.ImageData.Text
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Trial.TryPango
import Trial.MakePng

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 600 800
	cr <- cairoCreate sr
	drawFont cr 16 16 (normalFont "sans") "abcあいう"
	drawFont cr 16 64 (normalFont "serif") {
		fontStyle = StyleNormal } "abcあいう"
	drawFont cr 16 112 (normalFont "serif") {
		fontStyle = StyleOblique } "abcあいう"
	drawFont cr 16 160 (normalFont "serif") {
		fontStyle = StyleItalic,
		fontVariant = VariantSmallCaps } "abcあいう"
	drawFont cr 16 208 (normalFont "soulcraft") "abcあいう"
	drawFont cr 16 256 (normalFont "soulcraft") {
		fontSize = FontSize 48 } "abcあいう"

	drawFont cr 16 336 (normalFont "sans") {
		fontStretch = StretchUltraCondensed } "abcあいう"
	drawFont cr 16 384 (normalFont "sans") {
		fontStretch = StretchUltraExpanded } "abcあいう"

	drawFont cr 16 512 (VariableFont "Source Han Sans VF" (FontSize 32)) "abcあいう"

	for_ (zip [0 ..] [WeightThin .. WeightUltraheavy]) \(i :: Int, w) ->
		drawFont cr 316 (16 + 48 * fromIntegral i)
			(normalFont "sans") { fontWeight = w }
			"abcあいう"
	makePng sr "pngs/try-font.png"

normalFont :: String -> Font
normalFont ff = Font {
	fontFamily = ff,
	fontSize = FontSize 32,
	fontStyle = StyleNormal,
	fontVariant = VariantNormal,
	fontWeight = WeightNormal,
	fontStretch = StretchNormal }

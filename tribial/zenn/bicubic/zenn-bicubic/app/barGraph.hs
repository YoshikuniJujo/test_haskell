{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Stack
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import System.Environment

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

import Pixels

main :: HasCallStack => IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 500 400
	cr <- cairoCreate s

	(nm, f, cs) <- getArgs >>= \case
		["nearest"] -> pure ("Nearest", nearest, nearestColors)
		["linear"] -> pure ("Linear", linear, linearColors)
		["cubic"] -> pure ("Cubic", cubic, cubicColors)
		_ -> error "bad arguments"

	for_ (zip [0 ..] (cs !! 3)) \(i, c) -> do
		cairoSetSourceRgb cr c
		cairoRectangle cr (30 + i * 50) (300 - sumRgb c * 80) 50 (sumRgb c * 80)
		cairoFill cr

	let	fn = 15
		cs' = interpolate f
			[1, 1 + 1 / (3 * fn) .. 3] [1, 1 + 1 / 3 .. 3] colorsA

--	cairoMoveTo cr 50 (300 - sumRgb (cs' !! 3 !! 0) * 80)
	for_ (zip [0 ..] (cs' !! 3)) \(i, c) -> do
		let	x = (30 + (i / fn) * 50) + 25
			y = (300 - sumRgb c * 80)
		cairoLineTo cr x y
--	cairoLineTo cr (30 + fromIntegral (length (cs' !! 3) * 50) / 9) (300 - sumRgb (last $ cs' !! 3) * 80)
	cairoSet cr $ LineWidth 3
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoStroke cr

{-
	cairoSet cr $ LineWidth 8
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.8 0
	cairoMoveTo cr 20 20
	cairoLineTo cr 150 150
	cairoLineTo cr 20 280
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pl <- pangoCairoCreateLayout cr
	fd <- pangoFontDescriptionNew
	fd' <- pangoFontDescriptionFreeze fd
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl "あいうえお\nfoobar"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
-}

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng ("barGraph" ++ nm ++ ".png")
			$ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sumRgb :: Fractional d => Rgb d -> d
sumRgb (RgbDouble r g b) = r + g + b

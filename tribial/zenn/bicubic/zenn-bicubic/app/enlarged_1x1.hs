{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Maybe
import Data.Text qualified as T
import Data.Color
import Foreign.C.Types
import System.Environment

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Pixels
import Draw

margin :: Margin
margin = Margin {
	topMargin = 20, bottomMargin = 20, leftMargin = 20, rightMargin = 20 }

unit :: CDouble
unit = 30

number :: Int
number = 9

radius :: CDouble
radius = 5

radius' :: CDouble
radius' = 3

main :: IO ()
main = do
	(fp, cs) <- getArgs >>= \case
		["nearest"] -> pure ("nearest_1x1.png", nearestColors)
		["linear"] -> pure ("linear_1x1.png", linearColors)
		["cubic"] -> pure ("cubic_1x1.png", cubicColors)
		_ -> error "bad arguments"
	withPng' fp margin unit number \cr -> do
		forXyv_ (subtract 0 <$> distXs) (subtract 0 <$> distYs) (drop 3 $ take 4 <$> cs) \x y c -> do
			rectangle cr margin unit x y
			fill cr c
		grid cr margin unit number
		forXyv_ (subtract 0 <$> distXs) (subtract 0 <$> distYs) (drop 3 $ take 4 <$> cs) \x y c -> do
			circle cr margin unit x y radius'
			strokeAndFill cr 2 gray c
		forXyv_ srcXs srcYs (tail colors) \x y c -> do
			circle cr margin unit x y radius
			strokeAndFill cr 2 gray c

		cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
		pl <- pangoCairoCreateLayout cr
		fd <- pangoFontDescriptionNew
		fd' <- pangoFontDescriptionFreeze fd
		pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'

		pangoLayoutSet @T.Text pl "(0, 0)"
		cairoMoveTo cr (coordinateX margin unit 1 - unit / 1) (coordinateY margin unit 2 + unit / 3)
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		pangoLayoutSet @T.Text pl "(1, 0)"
		cairoMoveTo cr (coordinateX margin unit 2 - unit / 2) (coordinateY margin unit 2 + unit / 3)
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		pangoLayoutSet @T.Text pl "(0, 1)"
		cairoMoveTo cr (coordinateX margin unit 1 - unit / 1) (coordinateY margin unit 1 - unit * 5 / 4)
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		pangoLayoutSet @T.Text pl "(1, 1)"
		cairoMoveTo cr (coordinateX margin unit 2 - unit / 2) (coordinateY margin unit 1 - unit * 5 / 4)
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

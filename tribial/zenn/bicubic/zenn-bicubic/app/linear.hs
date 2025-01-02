{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths

import Data.Array

import Pixels
import Draw

leftMargin', rightMargin', topMargin', bottomMargin' :: CDouble
leftMargin' = 20; rightMargin' = 20; topMargin' = 20; bottomMargin' = 20

unit :: CDouble
unit = 30

number :: Int
number = 12

radius :: CDouble
radius = 5

radius' :: CDouble
radius' = 3

ceiling' :: (RealFrac x, Integral n) => x -> n
ceiling' = (+ 1) . floor

main :: IO ()
main = withPng "linear.png"
		(floor $ leftMargin' + unit * fromIntegral number + rightMargin')
		(floor $ topMargin' + unit * fromIntegral number + bottomMargin') \cr -> do

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	c00 = colorsA ! floor y ! floor x
				c10 = colorsA ! floor y ! ceiling' x
				c01 = colorsA ! ceiling' y ! floor x
				c11 = colorsA ! ceiling' y ! ceiling' x
				c0 = (c00 `mul` (fromInteger (ceiling' x) - x)) `add` (c10 `mul` (x - fromInteger (floor x)))
				c1 = (c01 `mul` (fromInteger (ceiling' x) - x)) `add` (c11 `mul` (x - fromInteger (floor x)))
				c = (c0 `mul` (fromInteger (ceiling' y) - y)) `add` (c1 `mul` (y - fromInteger (floor y)))
			cairoSetSourceRgb cr c
			cairoRectangle cr
				(leftMargin' + 3 * unit * (x - 1 / 6))
				(topMargin' + 3 * unit * (y - 1 / 6))
				(3 * unit * (1 / 3))
				(3 * unit * (1 / 3))
			cairoFill cr

	cairoSet cr $ LineWidth 1
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.6 0.6

	grid cr (Margin topMargin' bottomMargin' leftMargin' rightMargin') unit number

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	c00 = colorsA ! floor y ! floor x
				c10 = colorsA ! floor y ! ceiling' x
				c01 = colorsA ! ceiling' y ! floor x
				c11 = colorsA ! ceiling' y ! ceiling' x
				c0 = (c00 `mul` (fromInteger (ceiling' x) - x)) `add` (c10 `mul` (x - fromInteger (floor x)))
				c1 = (c01 `mul` (fromInteger (ceiling' x) - x)) `add` (c11 `mul` (x - fromInteger (floor x)))
				c = (c0 `mul` (fromInteger (ceiling' y) - y)) `add` (c1 `mul` (y - fromInteger (floor y)))
			cairoMoveTo cr
				(leftMargin' + 3 * unit * x + radius')
				(topMargin' + 3 * unit * y)
			cairoArc cr
				(leftMargin' + 3 * unit * x)
				(topMargin' + 3 * unit * y)
				radius' 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr c
			cairoFill cr

	for_ (zip [0 .. 4] colors) \(y, cs) ->
		for_ (zip [0 .. 4] cs) \(x, c) -> do
			cairoMoveTo cr
				(leftMargin' + 3 * unit * x + radius)
				(topMargin' + 3 * unit * y)
			cairoArc cr
				(leftMargin' + 3 * unit * x)
				(topMargin' + 3 * unit * y)
				radius 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr c
			cairoFill cr

mul :: Rgb CDouble -> CDouble -> Rgb CDouble
mul (RgbDouble r g b) n = fromJust $ rgbDouble (r * n) (g * n) (b * n)

add :: Rgb CDouble -> Rgb CDouble -> Rgb CDouble
add (RgbDouble r1 g1 b1)  (RgbDouble r2 g2 b2) = fromJust $ rgbDouble (r1 +! r2) (g1 +! g2) (b1 +! b2)

(+!) :: CDouble -> CDouble -> CDouble
a +! b	| a + b < 0 = 0
	| a + b < 1 = a + b
	| otherwise = a + b

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Stack
import Foreign.C.Types
import Data.Foldable
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths

import Data.Array

import Pixels
import Draw

leftMargin, rightMargin, topMargin, bottomMargin :: CDouble
leftMargin = 20; rightMargin = 20; topMargin = 20; bottomMargin = 20

unit :: CDouble
unit = 30

number :: CDouble
number = 12

radius :: CDouble
radius = 5

radius' :: CDouble
radius' = 3

main :: HasCallStack => IO ()
main = withPng "cubic.png"
		(floor $ leftMargin + unit * number + rightMargin)
		(floor $ topMargin + unit * number + bottomMargin) \cr -> do

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	(xi, xd) = properFraction x
				(yi, yd) = properFraction y
				css = (<$> [yi - 1 .. yi + 2]) \a ->
					(<$> [xi - 1 .. xi + 2]) \b -> colorsA ! a ! b
				css' = (<$> zip [-1 .. 2] css) \(yi', cs) ->
					(<$> zip [-1 .. 2] cs) \(xi', c) ->
						c `mul` (formula (abs $ yd - yi') * formula (abs $ xd - xi'))
			let	c = foldl add (0, 0, 0) $ concat css'
			cairoSetSourceRgb cr $ uncurry3 rgbDouble' c
			cairoRectangle cr
				(leftMargin + 3 * unit * (x - 1 / 6))
				(topMargin + 3 * unit * (y - 1 / 6))
				(3 * unit * (1 / 3))
				(3 * unit * (1 / 3))
			cairoFill cr

	cairoSet cr $ LineWidth 1
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.6 0.6

	for_ [0 .. number] \y -> do
		cairoMoveTo cr 0 (topMargin + unit * y)
		cairoLineTo cr (leftMargin + unit * number + rightMargin) (topMargin + unit * y)

	for_ [0 .. number] \x -> do
		cairoMoveTo cr (leftMargin + unit * x) 0
		cairoLineTo cr (leftMargin + unit * x) (topMargin + unit * number + bottomMargin)

	cairoStroke cr

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let	(xi, xd) = properFraction x
				(yi, yd) = properFraction y
				css = (<$> [yi - 1 .. yi + 2]) \a ->
					(<$> [xi - 1 .. xi + 2]) \b -> colorsA ! a ! b
				css' = (<$> zip [-1 .. 2] css) \(yi', cs) ->
					(<$> zip [-1 .. 2] cs) \(xi', c) ->
						c `mul` (formula (abs $ yd - yi') * formula (abs $ xd - xi'))
			let	c = foldl add (0, 0, 0) $ concat css'
			cairoMoveTo cr
				(leftMargin + 3 * unit * x + radius')
				(topMargin + 3 * unit * y)
			cairoArc cr
				(leftMargin + 3 * unit * x)
				(topMargin + 3 * unit * y)
				radius' 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr $ uncurry3 rgbDouble' c
			cairoFill cr

	for_ (zip [0 .. 4] colors) \(y, cs) ->
		for_ (zip [0 .. 4] cs) \(x, c) -> do
			cairoMoveTo cr
				(leftMargin + 3 * unit * x + radius)
				(topMargin + 3 * unit * y)
			cairoArc cr
				(leftMargin + 3 * unit * x)
				(topMargin + 3 * unit * y)
				radius 0 (2 * pi)
			cairoSet cr $ LineWidth 2
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
			cairoStrokePreserve cr
			cairoSetSourceRgb cr c
			cairoFill cr

formula :: CDouble -> CDouble
formula v_
	| v < 1 = (3 * v ^ (3 :: Int) - 5 * v ^ (2 :: Int) + 2) / 2
	| otherwise = (- v ^ (3 :: Int) + 5 * v ^ (2 :: Int) - 8 * v + 4) / 2
	where v = abs v_

mul :: HasCallStack => Rgb CDouble -> CDouble -> (CDouble, CDouble, CDouble)
mul (RgbDouble r g b) n = (r * n, g * n, b * n)

type RgbRaw = (CDouble, CDouble, CDouble)

add :: RgbRaw -> RgbRaw -> RgbRaw
add (r1, g1, b1)  (r2, g2, b2) = (r1 +! r2, g1 +! g2, b1 +! b2)

(+!) :: CDouble -> CDouble -> CDouble
a +! b	| a + b < 0 = 0
	| a + b < 1 = a + b
	| otherwise = a + b

rgbDouble' :: (HasCallStack, Ord d, Num d) => d -> d -> d -> Rgb d
rgbDouble' r g b = fromJust $ rgbDouble r' g' b'
	where
	to01 x	| x < 0 = 0
		| x > 1 = 1
		| otherwise = x
	r' = to01 r; g' = to01 g; b' = to01 b

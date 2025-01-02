{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.Array
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths

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

main :: IO ()
main = withPng "nearest.png"
		(floor $ leftMargin + unit * number + rightMargin)
		(floor $ topMargin + unit * number + bottomMargin) \cr -> do

	for_ [1, 1 + 1 / 3 .. 3] \y ->
		for_ [1, 1 + 1 / 3 .. 3] \x -> do
			let c = colorsA ! (round y) ! (round x)
			cairoSetSourceRgb cr c
			cairoRectangle cr
				(leftMargin + 3 * unit * (x - 1 / 6))
				(leftMargin + 3 * unit * (y - 1 / 6))
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
			let c = colorsA ! (round y) ! (round x)
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
			cairoSetSourceRgb cr c
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

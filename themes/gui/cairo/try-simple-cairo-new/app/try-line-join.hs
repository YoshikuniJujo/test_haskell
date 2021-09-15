{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths

import MakePng

main :: IO ()
main = pngWith "pngs/try-line-join.png" 768 896 \cr -> do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.05 0.1 0.05
	cairoSet cr $ LineWidth 32

	zipWithM_ (tryLineCap cr) [0 ..]
		[LineCapButt, LineCapRound, LineCapSquare]

	zipWithM_ (tryLineJoin cr) [0 ..]
		[LineJoinMiter, LineJoinRound, LineJoinBevel]

	cairoSet cr LineJoinMiter
	cairoMoveTo cr 270 320
	cairoLineTo cr 610 390
	cairoLineTo cr 270 390
	cairoStroke cr

	cairoSet cr LineJoinMiter
	cairoSet cr $ MiterLimit 9
	cairoMoveTo cr 270 450
	cairoLineTo cr 610 520
	cairoLineTo cr 270 520
	cairoStroke cr

tryLineCap :: CairoTIO s -> Int -> LineCap -> IO ()
tryLineCap cr i c = do
	cairoSet cr c
	cairoMoveTo cr 150 $ 80 * (fromIntegral i + 1)
	cairoLineTo cr 550 $ 80 * (fromIntegral i + 1)
	cairoStroke cr

tryLineJoin :: CairoTIO s -> Int -> LineJoin -> IO ()
tryLineJoin cr i c = do
	cairoSet cr c
	cairoMoveTo cr x1 y1
	cairoLineTo cr x2 y1
	cairoLineTo cr x2 y2
	cairoLineTo cr x1 y2
	cairoClosePath cr
	cairoStroke cr
	where
	[x1, x2] = (+ dx) <$> [80, 220]
	[y1, y2] = (+ dy) <$> [320, 450]
	dx = 448 * fromIntegral (i `div` 3)
	dy = 208 * fromIntegral (i `mod` 3)

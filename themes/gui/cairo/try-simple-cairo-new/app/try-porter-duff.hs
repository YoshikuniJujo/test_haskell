{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT
import Graphics.Cairo.Drawing.Paths
import MakePng

main :: IO ()
main = for_ (zip
	(("pngs/try-porter-duff-" ++) . (++ ".png") . show <$> [0 :: Int ..])
	(separate 12 $ operators ++ blendModes)) \(fp, os) -> samples fp os

samples :: FilePath -> [Operator] -> IO ()
samples fp ops = pngWith fp 768 896 \cr ->
	for_ (zip [0 ..] ops) \(i :: Int, op) ->
		tryPorterDuff cr
			(256 * fromIntegral (i `div` 4))
			(224 * fromIntegral (i `mod` 4))
			op

separate :: Int -> [a] -> [[a]]
separate _ [] = []
separate n xs = take n xs : separate n (drop n xs)

operators :: [Operator]
operators = [
	OperatorClear, OperatorSource, OperatorOver, OperatorIn, OperatorOut,
	OperatorAtop, OperatorDest, OperatorDestOver, OperatorDestIn,
	OperatorDestOut, OperatorDestAtop, OperatorXor, OperatorAdd,
	OperatorSaturate ]

blendModes :: [Operator]
blendModes = [
	OperatorMultiply, OperatorScreen, OperatorOverlay, OperatorDarken,
	OperatorLighten, OperatorColorDodge, OperatorColorBurn,
	OperatorHardLight, OperatorSoftLight, OperatorDifference,
	OperatorExclusion,
	OperatorHslHue, OperatorHslSaturation, OperatorHslColor,
	OperatorHslLuminosity ]

tryPorterDuff :: CairoTIO r -> CDouble -> CDouble -> Operator -> IO ()
tryPorterDuff cr x0 y0 op = do
	op0 <- cairoGet @Operator cr
	cairoRectangle cr l t 192 152
	cairoClip cr
	ichimatsu cr l t 192 152

	cairoPushGroup cr
	cairoRectangle cr l' t' 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.7 0 0 0.8
	cairoFill cr
	cairoSet cr op
	cairoRectangle cr (l' + 40) (t' + 30) 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0 0.9 0.4
	cairoFill cr
	cairoPopGroupToSource cr

	cairoPaint cr
	cairoResetClip cr
	cairoSet cr op0
	where l = x0 + 16; t = y0 + 16; l' = l + 16; t' = t + 16

ichimatsu :: CairoTIO s -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
ichimatsu cr l t w h =
	for_ (zip (cycle [0, 1]) [t, t + 32 .. t + h]) \(d, y) -> do
		for_ (zip (drop d $ cycle [grey, white]) [l, l + 32 .. l + w]) \(c, x) -> do
			cairoRectangle cr x y 32 32
			cairoSetSourceRgb cr c
			cairoFill cr



grey, white :: Rgb CDouble
grey = fromJust $ rgbDouble 0.7 0.7 0.7
white = fromJust $ rgbDouble 1 1 1


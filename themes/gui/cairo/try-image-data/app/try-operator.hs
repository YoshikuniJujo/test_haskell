{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.Maybe
import Data.Bool
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

main :: IO ()
main = do
	sr <- drawSurface Surface {
		sfcWidth = 768,
		sfcHeight = 896,
		surfaceClips = [
			Clip {	clipBounds = [[Rectangle 16 16 256 256]],
				clipDraws = [
					Draw {	drawOperator = OperatorOver,
						drawSource = Source . PatternColor . ColorRgba
							. fromJust $ rgbaDouble 0.15 0.3 0.05 1.0,
						drawMask = MaskPaint 1 } ] },
			Clip {	clipBounds = [],
				clipDraws = [
					Draw {	drawOperator = OperatorOver,
						drawSource = Source . PatternColor . ColorRgba
							. fromJust $ rgbaDouble 0.3 0.15 0.05 1.0,
						drawMask = MaskFill [Rectangle 32 32 64 64] },
					Draw {	drawOperator = OperatorClear,
						drawSource = Source . PatternColor . ColorRgba
							. fromJust $ rgbaDouble 0.15 0.05 0.3 1.0,
						drawMask = MaskFill [Rectangle 48 48 64 64] } ] },
			Clip {	clipBounds = [[Rectangle 256 256 256 192]],
				clipDraws = ichimatsu 16 16 100 100 },
			porterDuff 0 0 OperatorOver
			] }
	makePng sr "pngs/try-operator.png"

porterDuff :: Double -> Double -> Operator -> Clip 'Rgba
porterDuff x0 y0 op = Clip {
	clipBounds = [[Rectangle l t 192 152]],
	clipDraws = ichimatsu l t 100 100 ++ [
		]
	}
	where l = x0 + 16; t = y0 + 16; l' = l + 16; t' = t + 16

ichimatsu :: Double -> Double -> Int -> Int -> [Draw 'Rgba]
ichimatsu x0 y0 w h = (=<< [0 .. h]) \i -> (<$> [0 .. w]) \j ->
	bool darkSquare snowSquare (drop i (cycle [False, True]) !! j)
		(x0 + fromIntegral j * ichimatsuSize) (y0 + fromIntegral i * ichimatsuSize)

ichimatsuSize :: Double
ichimatsuSize = 32

darkSquare, snowSquare :: Double -> Double -> Draw 'Rgba
darkSquare = graySquare 0.6 ichimatsuSize
snowSquare = graySquare 0.7 ichimatsuSize

graySquare :: CDouble -> Double -> Double -> Double -> Draw 'Rgba
graySquare l s x y = Draw {
	drawOperator = OperatorOver,
	drawSource = Source . PatternColor . ColorRgba
		. fromJust $ rgbaDouble l l l 1.0,
	drawMask = MaskFill [Rectangle x y s s] }

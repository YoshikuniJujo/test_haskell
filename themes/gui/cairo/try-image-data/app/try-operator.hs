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
			Clip {	clipBounds = [[Rectangle 256 256 256 192]], clipDraws = ichimatsu 100 100 }
			] }
	makePng sr "pngs/try-operator.png"

ichimatsu :: Int -> Int -> [Draw 'Rgba]
ichimatsu w h = (=<< [0 .. h]) \i -> (<$> [0 .. w]) \j ->
	bool darkSquare snowSquare (drop i (cycle [False, True]) !! j)
		(fromIntegral j * ichimatsuSize) (fromIntegral i * ichimatsuSize)

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

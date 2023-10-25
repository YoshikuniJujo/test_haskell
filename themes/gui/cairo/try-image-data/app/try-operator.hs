{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.Maybe
import Data.Bool
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

import Data.CairoImage.Internal

main :: IO ()
main = for_ (zip [0 :: Int ..] $ separate 12 operators) \(i, ops) ->
	page ("pngs/try-operator-" ++ show i ++ ".png") ops

page :: FilePath -> [Operator] -> IO ()
page fp ops = do
	sr <- makeSurface Surface {
		surfaceBase = SurfaceBaseBlank {
			surfaceBaseWidth = 768,
			surfaceBaseHeight = 896 }, 
		surfaceClips = (<$> zip [0 ..] ops) \(i :: Int, op) ->
			porterDuff
				(256 * fromIntegral (i `div` 4))
				(224 * fromIntegral (i `mod` 4)) op
		}
	makePng sr fp

separate :: Int -> [a] -> [[a]]
separate n = \case [] -> []; xs -> take n xs : separate n (drop n xs)

operators :: [Operator]
operators = [OperatorClear .. OperatorHslLuminosity]

porterDuff :: Double -> Double -> Operator -> Clip 'Rgba
porterDuff x0 y0 op = Clip {
	clipBounds = [Bound FillRuleWinding [Rectangle l t 192 152]],
	clipDraws = ichimatsu l t 100 100 ++ [
		Draw {
			drawOperator = OperatorOver,
			drawSource = Source $ PatternNonSolid PatternFilterGood
				PatternExtendNone
				(Transform 1 0 0 1 (- l') (- t'))
				(PatternSurface $ sample op),
			drawMask = MaskPaint 1 }
		]
	}
	where l = x0 + 16; t = y0 + 16; l' = l + 16; t' = t + 16

sample :: Operator -> Surface 'Rgba
sample op = Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = 192, surfaceBaseHeight = 152 },
	surfaceClips = [
		Clip { clipBounds = [], clipDraws = [
			Draw {
				drawOperator = OperatorOver,
				drawSource = Source . PatternSolid . ColorRgba
					. fromJust $ rgbaDouble 0.7 0 0 0.8,
				drawMask = MaskFill FillRuleWinding [Rectangle 0 0 120 90] },
			Draw {
				drawOperator = op,
				drawSource = Source . PatternSolid . ColorRgba
					. fromJust $ rgbaDouble 0 0 0.9 0.4,
				drawMask = MaskFill FillRuleWinding [Rectangle 40 30 120 90] }
			] } ] }

ichimatsu :: Double -> Double -> Int -> Int -> [Draw 'Rgba]
ichimatsu x0 y0 w h = (=<< [0 .. h]) \i -> (<$> [0 .. w]) \j ->
	bool darkSquare snowSquare (drop i (cycle [False, True]) !! j)
		(x0 + fromIntegral j * ichimatsuSize) (y0 + fromIntegral i * ichimatsuSize)

ichimatsuSize :: Double
ichimatsuSize = 32

darkSquare, snowSquare :: Double -> Double -> Draw 'Rgba
darkSquare = graySquare 0.6 ichimatsuSize
snowSquare = graySquare 0.7 ichimatsuSize

graySquare :: Double -> Double -> Double -> Double -> Draw 'Rgba
graySquare l s x y = Draw {
	drawOperator = OperatorOver,
	drawSource = Source . PatternSolid . ColorRgba
		. fromJust $ rgbaDouble l l l 1.0,
	drawMask = MaskFill FillRuleWinding [Rectangle x y s s] }

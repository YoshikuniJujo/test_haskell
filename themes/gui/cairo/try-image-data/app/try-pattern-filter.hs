{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.CairoImage
import Data.ImageData
import System.Environment

import Trial.TryCairo
import Trial.MakePng

import Data.CairoImage.Internal

imgSize :: Integer
imgSize = 256

main :: IO ()
main = do
	ptnSize : _ <- (read <$>) <$> getArgs
	(`makePng` "pngs/try-pattern-filter.png") =<< makeSurface Surface {
		surfaceBase = SurfaceBaseBlank {
			surfaceBaseWidth = imgSize * 2, surfaceBaseHeight = imgSize * 3 },
		surfaceClips = [
			clip imgSize ptnSize PatternFilterFast 0 0,
			clip imgSize ptnSize PatternFilterGood 1 0,
			clip imgSize ptnSize PatternFilterBest 0 1,
			clip imgSize ptnSize PatternFilterNearest 1 1,
			clip imgSize ptnSize PatternFilterBilinear 0 2 ] }

clip :: Integer -> Integer -> PatternFilter -> Integer -> Integer -> Clip 'Rgba
clip is ps pf x y = Clip {
	clipBounds = [Bound FillRuleWinding [
		Rectangle
			(fromInteger $ is * x) (fromInteger $ is * y)
			(fromInteger is) (fromInteger is) ]],
	clipDraws = [
		Draw {	drawOperator = OperatorOver,
			drawSource = Source . PatternNonSolid pf
					PatternExtendNone
					(Transform rt 0 0 rt
						(fromInteger $ - ps * x)
						(fromInteger $ - ps * y))
				. PatternSurface . sfc $ fromInteger ps,
			drawMask = MaskPaint 1 } ] }
	where rt = fromIntegral ps / fromIntegral is

sfc :: CInt -> Surface 'Rgba
sfc sz = Surface { surfaceBase = SurfaceBaseArgb32 $ img sz, surfaceClips = [] }

img :: CInt -> Argb32
img sz = generateImage sz sz (color sz)

color :: CInt -> CInt -> CInt -> PixelArgb32
color sz x y
	| x < hf && y < hf = red
	| x >= hf && y < hf = blue
	| x < hf && y >= hf = yellow
	| otherwise = green
	where hf = sz `div` 2

red, green, blue, yellow :: PixelArgb32
red = PixelArgb32Straight 255 127 51 25
green = PixelArgb32Straight 255 51 127 25
blue = PixelArgb32Straight 255 25 51 85
yellow = PixelArgb32Straight 255 127 127 25

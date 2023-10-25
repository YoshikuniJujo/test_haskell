{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.CairoImage
import Data.ImageData

import Trial.TryCairo
import Trial.MakePng

import Data.CairoImage.Internal

imgSize, ptnSize :: Integer
imgSize = 256
ptnSize = 64

main :: IO ()
main = (`makePng` "pngs/try-pattern-extend.png") =<< makeSurface Surface {
	surfaceBase = SurfaceBaseBlank {
		surfaceBaseWidth = imgSize * 2, surfaceBaseHeight = imgSize * 3 },
	surfaceClips = [
		clip1 PatternExtendNone 0 0,
		clip1 PatternExtendRepeat 1 0,
		clip1 PatternExtendReflect 0 1,
		clip1 PatternExtendPad 1 1,
		clip2 PatternExtendReflect 0 2,
		clip2 PatternExtendPad 1 2 ] }

clip1, clip2 :: PatternExtend -> Integer -> Integer -> Clip 'Rgba
clip1 = clip imgSize ptnSize color1
clip2 = clip imgSize ptnSize color2

clip :: Integer -> Integer -> MakePixel -> PatternExtend -> Integer -> Integer -> Clip 'Rgba
clip is ps clr pe x y = Clip {
	clipBounds = [Bound FillRuleWinding [
		Rectangle
			(fromInteger $ is * x) (fromInteger $ is * y)
			(fromInteger is) (fromInteger is) ]],
	clipDraws = [
		Draw {	drawOperator = OperatorOver,
			drawSource = Source
				. PatternNonSolid PatternFilterGood pe
					(Transform 1 0 0 1
						(fromInteger $ - is * x - ps)
						(fromInteger $ - is * y - ps))
				. PatternSurface $ sfc (fromInteger ps) clr,
			drawMask = MaskPaint 1 } ] }

sfc :: CInt -> MakePixel -> Surface 'Rgba
sfc sz clr = Surface { surfaceBase = SurfaceBaseArgb32 $ img sz clr, surfaceClips = [] }

img :: CInt -> MakePixel -> Argb32
img sz clr = generateImage sz sz (clr sz)

type MakePixel = CInt -> CInt -> CInt -> PixelArgb32

color1, color2 :: MakePixel
color1 sz x y
	| x < hf && y < hf = red
	| x >= hf && y < hf = blue
	| x < hf && y >= hf = yellow
	| otherwise = green
	where hf = sz `div` 2

color2 sz x y
	| a < sz && b < 0 = red
	| a >= sz && b < 0 = blue
	| a < sz && b >= 0 = yellow
	| otherwise = green
	where
	a = x + y
	b = x - y

red, green, blue, yellow :: PixelArgb32
red = PixelArgb32Straight 255 127 51 25
green = PixelArgb32Straight 255 51 127 25
blue = PixelArgb32Straight 255 25 51 85
yellow = PixelArgb32Straight 255 127 127 25

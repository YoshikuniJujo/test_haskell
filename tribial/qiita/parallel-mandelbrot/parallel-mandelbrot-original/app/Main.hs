{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Word
import Data.Complex
import Codec.Picture

trial :: Word8
trial = 255

black, white :: Pixel8
black = 0
white = 255

width, height :: Int
width = 1024
height = 1024
-- width = 256
-- height = 256

lefttop, rightbottom :: Complex Double
-- lefttop = (- 1.20) :+ 0.35
-- rightbottom = (- 1.0) :+ 0.15
-- lefttop = (- 0.875) :+ 0.25
-- rightbottom = (- 0.625) :+ 0
lefttop = (- 1.5) :+ 1
rightbottom = 0.5 :+ (- 1)

main :: IO ()
main = writePng "mandelbrot.png"
	$ generateImage (((toPixel . result) .) . toComplex) width height
	

result :: Complex Double -> Maybe Word8
result c = case dropWhile (check . snd)
		$ zip [0 .. trial - 1] (tail $ iterate (step c) 0) of
	[] -> Nothing
	(i, _) : _ -> Just i

check :: Complex Double -> Bool
check z = magnitude z <= 2

step :: Complex Double -> Complex Double -> Complex Double
step c z = z * z + c

toPixel :: Maybe Word8 -> Pixel8
toPixel Nothing = black
toPixel (Just i) = white - i

toComplex :: Int -> Int -> Complex Double
toComplex x y =
	(l + fromIntegral x * w / fromIntegral width) :+
	(t - fromIntegral y * h / fromIntegral height)
	where
	w = r - l
	h = t - b
	l :+ t = lefttop
	r :+ b = rightbottom

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Word (Word8)
import Data.Complex (Complex(..), magnitude)
import Codec.Picture (Pixel8, writePng, generateImage)

trial :: Word8
trial = 255

black, white :: Pixel8
black = 0; white = 255

width, height :: Int
width = 1024; height = 1024

lefttop, rightbottom :: Complex Double
lefttop = (- 1.20) :+ 0.40; rightbottom = (- 1.00) :+ 0.20

main :: IO ()
main = writePng "mandelbrot.png"
	$ generateImage (((toPixel . try) .) . toComplex) width height

try :: Complex Double -> Maybe Word8
try c = case dropWhile (check . snd)
		$ zip [0 .. trial - 1] (tail $ iterate step 0) of
	[] -> Nothing
	(i, _) : _ -> Just i
	where
	check z = magnitude z <= 2
	step z = z * z + c

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

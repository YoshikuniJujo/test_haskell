{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Codec.Picture

main :: IO ()
main = writePng "chibi-mario.png" $ generateImage pixel 325 425

pixel :: Int -> Int -> PixelRGBA8
pixel x y
	| x == 0 || y == 0 = PixelRGBA8 0 0 0 0
	| x < 13, y < 17 = toRgba8 $ chibiMario !! (y - 1) !! (x - 1)
	| otherwise = PixelRGBA8 0 0 0 0

data Color = T | R | G | Y deriving Show

toRgba8 :: Color -> PixelRGBA8
toRgba8 T = PixelRGBA8 0 0 0 0
toRgba8 R = PixelRGBA8 153 0 0 255
toRgba8 G = PixelRGBA8 0 85 0 255
toRgba8 Y = PixelRGBA8 204 127 0 255

chibiMario :: [[Color]]
chibiMario = [
	[T, T, T, R, R, R, R, R, T, T, T, T],
	[T, T, R, R, R, R, R, R, R, R, R, T],
	[T, T, G, G, G, Y, Y, G, Y, T, T, T],
	[T, G, Y, G, Y, Y, Y, G, Y, Y, Y, T],
	[T, G, Y, G, G, Y, Y, Y, G, Y, Y, Y],
	[T, G, G, Y, Y, Y, Y, G, G, G, G, T],
	[T, T, T, Y, Y, Y, Y, Y, Y, Y, T, T],
	[T, T, G, G, R, G, G, G, T, T, T, T],
	[T, G, G, G, R, G, G, R, G, G, G, T],
	[G, G, G, G, R, R, R, R, G, G, G, G],
	[Y, Y, G, R, Y, R, R, Y, R, G, Y, Y],
	[Y, Y, Y, R, R, R, R, R, R, Y, Y, Y],
	[Y, Y, R, R, R, R, R, R, R, R, Y, Y],
	[T, T, R, R, R, T, T, R, R, R, T, T],
	[T, G, G, G, T, T, T, T, G, G, G, T],
	[G, G, G, G, T, T, T, T, G, G, G, G]
	]

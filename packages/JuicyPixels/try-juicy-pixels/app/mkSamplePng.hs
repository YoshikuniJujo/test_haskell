{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Codec.Picture

main :: IO ()
main = do
	writePng "sample.png" $ generateImage color 64 64
	writePng "sample16x16.png" $ generateImage color2 16 16
	writePng "sample2x2.png" $ generateImage color3 2 2

color :: Int -> Int -> PixelRGBA8
color x y = PixelRGBA8 r (0xff - r) 0x00 0xff
	where r = fromIntegral $ (x + y) * 2

color2 :: Int -> Int -> PixelRGBA8
color2 x y = PixelRGBA8 r (0xff - r) 0x00 0xff
	where r = fromIntegral $ (x + y) * 8

color3 :: Int -> Int -> PixelRGBA8
color3 0 0 = PixelRGBA8 0xff 0x00 0x00 0xff
color3 1 0 = PixelRGBA8 0x00 0xff 0x00 0xff
color3 0 1 = PixelRGBA8 0xff 0xff 0x00 0xff
color3 1 1 = PixelRGBA8 0x00 0x00 0xff 0xff
color3 _ _ = error "bad"

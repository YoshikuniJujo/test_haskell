{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Codec.Picture

main :: IO ()
main = writePng "sample.png" $ generateImage color 64 64

color :: Int -> Int -> PixelRGBA8
color x y = PixelRGBA8 r (0xff - r) 0x00 0xff
	where r = fromIntegral $ (x + y) * 2

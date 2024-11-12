{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Codec.Picture

main :: IO ()
main = writePng "xy_rg.png" $ generateImage
		(\(fromIntegral -> x) (fromIntegral -> y) ->
	PixelRGBA8 x y (255 - x `div` 2 - y `div` 2) 255) 256 256

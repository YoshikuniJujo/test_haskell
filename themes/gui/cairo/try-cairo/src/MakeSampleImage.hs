{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MakeSampleImage where

import Data.Word
import Codec.Picture

red100 :: Image PixelRGBA8
red100 = generateImage (\_ _ -> PixelRGBA8 0xff 0 0 0xff) 500 500

makeRed100Png :: IO ()
makeRed100Png = savePngImage "red100.png" $ ImageRGBA8 red100

roundAlpha :: Image PixelRGBA8
roundAlpha = generateImage (\x y -> PixelRGBA8 0xff 0 0 (circle 200 250 250 x y)) 500 500

makeRoundAlpha :: IO ()
makeRoundAlpha = savePngImage "roundAlpha.png" $ ImageRGBA8 roundAlpha

circle :: Int -> Int -> Int -> Int -> Int -> Word8
circle (fromIntegral -> r) (fromIntegral -> x0) (fromIntegral -> y0) (fromIntegral -> x) (fromIntegral -> y) =
	round $ z * 0xff / r
	where z = sqrt' $ r ^ (2 :: Int) - (x - x0) ^ (2 :: Int) - (y - y0) ^ (2 :: Int)

sqrt' :: Double -> Double
sqrt' x | x <= 0 = 0
sqrt' x = sqrt x

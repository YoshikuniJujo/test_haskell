{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Data.Set
import Data.Bool
import Codec.Picture

dot :: Double -> (Double, Double)
dot x = (
	200 * cos x - 120 * sin (18 / 17 * x) + 400,
	200 * sin x - 120 * cos (18 / 17 * x) + 400 )

dot2 :: Double -> (Double, Double)
dot2 x = (
	200 * cos x - 140 * sin (17 / 16 * x) + 400,
	200 * sin x - 140 * cos (17 / 16 * x) + 400 )

dots :: Set (Int, Int)
dots = fromList $ ((round *** round) . dot) <$> [0, pi / 1024 .. 128 * pi]

dots2 :: Set (Int, Int)
dots2 = fromList $ ((round *** round) . dot2) <$> [0, pi / 1024 .. 128 * pi]

pixel :: Int -> Int -> PixelRGB8
pixel x y
--	| member (x, y) dots2 = PixelRGB8 0xff 0x00 0x00
--	| member (x, y) dots = PixelRGB8 0x00 0xff 0x00
	| member (x, y) dots = PixelRGB8 0x7f 0x7f 0x00
	| otherwise = PixelRGB8 0xff 0xff 0xff

img :: Image PixelRGB8
img = generateImage pixel 800 800

writeTrochoid :: IO ()
writeTrochoid = writePng "tmp.png" img

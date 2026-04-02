{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Int
import System.Environment

import Waveform

import WriteMonoral16

main :: IO ()
main = do
	nm : (read -> hz) : (read -> l) : _ <- getArgs
	print $ waveFormat (monoral hz l)
	writeWave (nm ++ ".png") (monoral hz l) 0 Nothing 0.02 0.01
	putMonoral16 (nm ++ ".wav") (monoral hz l)

monoral :: Double -> Double -> Monoral16
monoral hz l = Monoral16 {
	waveFormat = sampleWaveFormatEx,
	waveData = (round <$>) . zipWith (*) (mask l)
		$ foldr (zipWith (+)) (repeat 0) [
			sinWave hz 6000,
			sinWave (hz * 2) 2000,
			sinWave (hz * 3) 700,
			sinWave (hz * 4) 200
			]
	}

mask :: Double -> [Double]
mask l = [0, 12 / 48000 .. 1] ++ [1, 1 - 1 / (l * 48000) .. 0]

sinWave :: Double -> Double -> [Double]
sinWave hz l = (* l) . sin <$> [0, (2 * pi) * hz / 48000 ..]

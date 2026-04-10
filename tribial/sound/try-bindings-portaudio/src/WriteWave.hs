{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WriteWave (

	monoral, putMonoral16

	) where

import Waveform

import WriteMonoral16

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

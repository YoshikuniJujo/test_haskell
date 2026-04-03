{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment

import Waveform

import WriteMonoral16

main :: IO ()
main = do
	nm : (read -> hz1) : (read -> sz1) :
		(read -> hz2) : (read -> sz2) : (read ->l) : _ <- getArgs
	print $ waveFormat (monoral hz1 sz1 hz2 sz2 l)
	writeWave (nm ++ ".png") (monoral hz1 sz1 hz2 sz2 l) 0 Nothing 0.02 0.01
	putMonoral16 (nm ++ ".wav") (monoral hz1 sz1 hz2 sz2 l)

monoral :: Double -> Double -> Double -> Double -> Double -> Monoral16
monoral hz1 sz1 hz2 sz2 l = Monoral16 {
	waveFormat = sampleWaveFormatEx,
	waveData = (round <$>) . zipWith (*) (mask l)
		$ foldr (zipWith (+)) (repeat 0) [
			sinWave hz1 sz1,
			sinWave hz2 sz2 ]
	}

mask :: Double -> [Double]
mask l = [0, 12 / 48000 .. 1] ++ [1, 1 - 1 / (l * 48000) .. 0]

sinWave :: Double -> Double -> [Double]
sinWave hz l = (* l) . sin <$> [0, (2 * pi) * hz / 48000 ..]

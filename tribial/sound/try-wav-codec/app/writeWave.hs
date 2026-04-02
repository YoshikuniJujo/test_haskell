{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Int
import System.Environment

import Waveform

import WriteMonoral16

main :: IO ()
main = do
	nm : (read -> hz) : _ <- getArgs
	print $ waveFormat (monoral hz)
	writeWave (nm ++ ".png") (monoral hz) 0 Nothing 0.02 0.01
	putMonoral16 (nm ++ ".wav") (monoral hz)

monoral :: Double -> Monoral16
monoral hz = Monoral16 {
	waveFormat = sampleWaveFormatEx,
	waveData = round <$> zipWith (*) mask (sinWave hz)
	}

mask :: [Double]
mask = [0, 12 / 48000 .. 1] ++ [1, 1 - 1 / (48000 - 4800) .. 0]

sinWave :: Double -> [Double]
sinWave hz = (* 24000) . sin <$> [0, (2 * pi) * hz / 48000 ..]

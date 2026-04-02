module Main (main) where

import Data.Int
import Waveform

main :: IO ()
main = do
	print $ waveFormat monoral
	writeWave "sample.png" monoral 0 Nothing 0.02 0.02

monoral :: Monoral16
monoral = Monoral16 {
	waveFormat = sampleWaveFormatEx,
	waveData = round <$> zipWith (*) mask sinWave
	}

mask :: [Double]
mask = [0, 12 / 48000 .. 1] ++ [1, 1 - 1 / (48000 - 4800) .. 0]

sinWave :: [Double]
sinWave = (* 6000) . sin <$> [0, (2 * pi) * 440 / 48000 .. 880 * pi]

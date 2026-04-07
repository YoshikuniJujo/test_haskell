{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hz where

import Data.Vector qualified as V

waveform :: Double -> V.Vector Float
waveform hz = V.fromList [ sin $ realToFrac t |
	i <- [0 .. period - 1],
	let	t = i * hz / samplingRate * 2 * pi ]
	where
	period = samplingRate / hz

samplingRate :: Double
samplingRate = 48000

la :: Double
la = 440

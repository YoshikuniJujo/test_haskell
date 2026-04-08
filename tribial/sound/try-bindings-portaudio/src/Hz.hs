{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hz where

import Data.Vector qualified as V

type Hz = Double

waveform :: Double -> V.Vector Float
waveform 0 = V.fromList [0]
waveform hz = V.fromList [ sin $ realToFrac t |
	i <- [0 .. period hz - 1],
	let	t = i * hz / samplingRate * 2 * pi ]

samplingRate :: Double
samplingRate = 48000

period :: Hz -> Double
period 0 = 1
period hz = samplingRate / hz

lla, lti, doo, re, mi, fa, so, la, ti, hdo :: Hz
lla = 220; lti = 247; doo = 262; re = 294; mi = 330
fa = 349; so = 392; la = 440; ti = 494; hdo = 523

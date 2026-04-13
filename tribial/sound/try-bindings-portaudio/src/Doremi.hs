{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Doremi where

import Data.Vector qualified as V
import Data.Map qualified as Map

data Doremi = Hz Float deriving (Show, Eq, Ord)

semitone :: Int -> Doremi -> Doremi
semitone n d = d `mul` (2 ** (fromIntegral n / 12))

octave :: Int -> Doremi -> Doremi
octave n d = d `mul` (2 ^ n)

mul :: Doremi -> Float -> Doremi
Hz d `mul` m = Hz $ d * m

fromHz :: Double -> Doremi
fromHz = Hz . realToFrac

ldo, lre, lmi, lfa, lso, lla, lti :: Doremi
ldo = fromHz 131; lre = fromHz 147; lmi = fromHz 165; lfa = fromHz 175;
lso = fromHz 196; lla = fromHz 220; lti = fromHz 247

doo, re, mi, fa, so, la, ti :: Doremi
doo = fromHz 262; re = fromHz 294; mi = fromHz 330; fa = fromHz 349
so = fromHz 392; la = fromHz 440; ti = fromHz 494

hdo, hre :: Doremi
hdo = fromHz 523; hre = fromHz 587

whole :: [Doremi]
whole = []

waveform :: Map.Map Doremi (V.Vector Float)
waveform = Map.fromList . zip whole $ hzWaveform <$> []

soundPressure' :: Doremi -> Int -> Float
soundPressure' (Hz hz) phs =
	wave0 $ fromIntegral phs * hz / realToFrac samplingRate * 2 * pi
	-- wf V.! (phs `mod` V.length wf)

hzWaveform :: Double -> V.Vector Float
hzWaveform 0 = V.fromList [0]
-- waveform hz = V.fromList [ sin $ realToFrac t |
hzWaveform hz = V.fromList [ wave0 $ realToFrac t |
	i <- [0 .. period hz - 0.9],
	let	t = i * hz / samplingRate * 2 * pi ]

wave0 :: Float -> Float
wave0 x = 0.45 * sin x + 0.15 * sin (2 * x) + 0.05 * sin (3 * x)

samplingRate :: Double
samplingRate = 48000

period :: Hz -> Double
period 0 = 1
period hz = samplingRate / hz

type Hz = Double

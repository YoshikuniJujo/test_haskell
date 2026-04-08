{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Doremi where

import Data.Vector qualified as V
import Data.Map qualified as Map
import Hz qualified as Hz

data Doremi = LLa | LTi | Do | Re | Mi | Fa | So | La | Ti | HDo
	deriving (Show, Eq, Ord, Enum)

toHz :: Doremi -> Double
toHz = \case
	LLa -> Hz.lla; LTi -> Hz.lti; Do -> Hz.doo; Re -> Hz.re; Mi -> Hz.mi
	Fa -> Hz.fa; So -> Hz.so; La -> Hz.la; Ti -> Hz.ti; HDo -> Hz.hdo

waveform :: Map.Map Doremi (V.Vector Float)
waveform = Map.fromList . zip [LLa ..] $ Hz.waveform <$> [
	Hz.lla, Hz.lti, Hz.doo, Hz.re, Hz.mi,
	Hz.fa, Hz.so, Hz.la, Hz.ti, Hz.hdo ]

soundPressure :: Doremi -> Int -> Float
soundPressure nt phs = realToFrac $ sin (hz * fromIntegral phs / Hz.samplingRate * 2 * pi)
	where hz = toHz nt

soundPressure' :: Doremi -> Int -> Float
soundPressure' nt phs = wf V.! (phs `mod` V.length wf)
	where
	wf = waveform Map.! nt

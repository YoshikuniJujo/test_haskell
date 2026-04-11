{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Doremi where

import Data.Vector qualified as V
import Data.Map qualified as Map
import Hz qualified as Hz

data Doremi = Hz (V.Vector Float) deriving (Show, Eq, Ord)

fromHz :: Double -> Doremi
fromHz = Hz . Hz.waveform

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
waveform = Map.fromList . zip whole $ Hz.waveform <$> []

soundPressure' :: Doremi -> Int -> Float
soundPressure' (Hz wf) phs = wf V.! (phs `mod` V.length wf)

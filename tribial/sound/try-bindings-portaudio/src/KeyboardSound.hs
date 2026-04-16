{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyboardSound where

import Data.Word
import System.Clock

import Keyboard
import Sound
import Doremi

noteToChanger :: TimeSpec -> NoteEvent -> Maybe (Int, Sound.Event)
noteToChanger tm0 (tm, Note _ n v) = let
	i = toNanoSecs (tm - tm0) * 48000 `div` 1000000000
	(d, t) = case v of
		0 -> (- 1 / 36000, 0)
		_ -> (1 / 1000, 0.7) in
	(\drm -> (fromIntegral i, (drm, d, t))) <$> noteToDoremi n
noteToChanger _ (_, NoNote _ _ _) = Nothing

{-
noteToDoremi :: Word8 -> Maybe Doremi
noteToDoremi = (`lookup` noteDoremiTable)
-}

noteToDoremi :: Word8 -> Maybe Doremi
noteToDoremi n = Just . Hz $ 440 * 2 ** ((fromIntegral n - 69) / 12)

noteDoremiTable :: [(Word8, Doremi)]
noteDoremiTable = [
	(72, hdo),
	(74, hre)
	]

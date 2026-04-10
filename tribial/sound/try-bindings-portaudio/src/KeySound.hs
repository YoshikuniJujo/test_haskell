{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeySound where

import Data.Maybe
import KeyEvent
import Sound
import Doremi

keyLogToChangers :: Double -> [KeyAction] -> [(Int, Sound.Event)]
keyLogToChangers tm0 = mapMaybe $ keyActionToChanger tm0

keyActionToChanger :: Double -> KeyAction -> Maybe (Int, Sound.Event)
keyActionToChanger tm0 KeyAction {
	keyActionKey = k, keyActionTime = tm, keyActionAction = pr } = let
	n = (tm - tm0) * 48000
	(d, t) = case pr of
		Press -> (1 / 1000, 0.7)
		Release -> (- 1 / 36000, 0) in (\nt -> (round n, (nt, d, t))) <$> keyToDoremi k

keyToDoremi :: Key -> Maybe Doremi
keyToDoremi = (`lookup` keyDoremiTable)

keyDoremiTable :: [(Key, Doremi)]
keyDoremiTable =
	[	Key'A, Key'S, Key'D, Key'F, Key'G,
		Key'H, Key'J, Key'K, Key'L, Key'Semicolon, Key'Apostrophe
		] `zip` [LLa, LTi, Do, Re, Mi, Fa, So, La, Ti, HDo, HRe]

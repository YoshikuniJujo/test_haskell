{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeySound (

	keyLogToChangers,
	keyActionToChangers,
	) where

import KeyEvent
import Sound
import Doremi

keyLogToChangers :: Double -> [KeyAction] -> [(Int, Sound.Event)]
keyLogToChangers tm0 = concatMap $ keyActionToChangers' tm0

keyActionToChangers :: Double -> KeyAction -> [(Int, Sound.Event)]
keyActionToChangers tm0 = keyActionToChangers' tm0

keyActionToChangers' :: Double -> KeyAction -> [(Int, Sound.Event)]
keyActionToChangers' tm0 KeyAction {
	keyActionKey = k, keyActionTime = tm, keyActionAction = pr } = let
	n = (tm - tm0) * 48000
	(d, t) = case pr of
		Press -> (1 / 1000, 0.7)
		Release -> (- 1 / 36000, 0) in (\nt -> (round n, (nt, d, t))) <$> keyToDoremi' k

keyToDoremi' :: Key -> [Doremi]
keyToDoremi' = maybe [] id . (`lookup` keyDoremiTable2)

keyDoremiTable :: [(Key, Doremi)]
keyDoremiTable =
	[	Key'A, Key'S, Key'D, Key'F, Key'G,
		Key'H, Key'J, Key'K, Key'L, Key'Semicolon, Key'Apostrophe
		] `zip` [lla, lti, doo, re, mi, fa, so, la, ti, hdo, hre]

keyDoremiTable2 :: [(Key, [Doremi])]
keyDoremiTable2 = [
	Key'H, Key'Y, Key'Apostrophe,
	Key'Slash, Key'Period, Key'Comma, Key'M,
	Key'J, Key'K, Key'L, Key'O, Key'Semicolon,
	Key'P, Key'U, Key'I,

	Key'A, Key'S, Key'D, Key'E, Key'F, Key'R

	] `zip` ((((octave 1 . semitone 2) <$>) <$>) [

	[ldo], [lre], [lmi], [lfa], [lso], [lla], [lti],
	[doo], [re], [mi], [fa], [so], [la], [ti], [hdo]

	] ++ ((semitone 2 <$>) <$>) [

	[ldo], [lre], [lmi], [lfa], [lso], [lla]

	])

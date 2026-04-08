{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sound (

	Sound, noTouch

	) where

import Control.Arrow
import Data.Map qualified as Map
import Doremi
import Loudness

type Sound = Map.Map Doremi PhaseLoudness

removeZeros :: Sound -> Sound
removeZeros = Map.filter (not . zero . plLoudness)

data PhaseLoudness =
	PhaseLoudness { plPhase :: Int, plLoudness :: Loudness } deriving Show

nextPhaseLoudness :: PhaseLoudness -> PhaseLoudness
nextPhaseLoudness PhaseLoudness {
	plPhase = phs, plLoudness = l } = PhaseLoudness {
	plPhase = phs + 1, plLoudness = nextLoudness l }

currentPhaseLoudness :: PhaseLoudness -> (Int, Float)
currentPhaseLoudness = plPhase &&& currentLoudness . plLoudness

singleNote :: Doremi -> Int -> Float -> Float
singleNote nt phs l = soundPressure nt phs * l

uncurry' :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurry' = (uncurry $) . (uncurry .)

adjustLoudness :: Sound -> Float -> Float
adjustLoudness s | wl > 1 = (/ wl) | otherwise = id where wl = wholeLoudness s

wholeLoudness :: Sound -> Float
wholeLoudness = sum . (currentLoudness . plLoudness <$>) . Map.elems

uncons :: Sound -> ([(Doremi, (Int, Float))], Sound)
uncons =
	((currentPhaseLoudness `second`) <$>) . Map.toList &&&
	removeZeros . (nextPhaseLoudness <$>)

uncons' :: Sound -> (Float, Sound)
uncons' s = ((adjustLoudness s . sum . (uncurry' singleNote <$>)) `first`)
	$ uncons s

unfoldr' :: (b -> (a, b)) -> b -> Int -> ([a], b)
unfoldr' f s n
	| n < 1 = ([], s)
	| otherwise = let (x, s') = f s in (x :) `first` unfoldr' f s' (n - 1)

noTouch :: Sound -> Int -> ([Float], Sound)
noTouch = unfoldr' uncons'

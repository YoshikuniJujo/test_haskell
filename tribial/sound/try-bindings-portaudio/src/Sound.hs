{-# LANGUAGE ImportQualifiedPost #-}
{-# LAnGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sound (

	Sound, zeroSound, noTouch, sound

	) where

import Control.Arrow
import Data.List qualified as L
import Data.Map qualified as Map
import Doremi
import Loudness

type Sound = Map.Map Doremi PhaseLoudness

zeroSound :: Sound
zeroSound = Map.empty

removeZeros :: Sound -> Sound
removeZeros = Map.filter (not . zero . plLoudness)

data PhaseLoudness =
	PhaseLoudness { plPhase :: Int, plLoudness :: Loudness } deriving Show

nextPhaseLoudness :: PhaseLoudness -> PhaseLoudness
nextPhaseLoudness PhaseLoudness {
	plPhase = phs, plLoudness = l } = PhaseLoudness {
	plPhase = phs + 1, plLoudness = nextLoudness l }

{-
mapLoudness :: (Loudness -> Loudness) -> PhaseLoudness -> PhaseLoudness
mapLoudness f pl = pl { plLoudness = f $ plLoudness pl }
-}

mapLoudness' ::
	(Maybe Loudness -> Loudness) -> Maybe PhaseLoudness -> PhaseLoudness
mapLoudness' f = \case
	Nothing -> PhaseLoudness { plPhase = 0, plLoudness = f Nothing }
	Just pl -> pl { plLoudness = f . Just $ plLoudness pl }

currentPhaseLoudness :: PhaseLoudness -> (Int, Float)
currentPhaseLoudness = plPhase &&& currentLoudness . plLoudness

singleNote :: Doremi -> Int -> Float -> Float
singleNote nt phs l = soundPressure' nt phs * l

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

event :: Sound -> Doremi -> Float -> Float -> Sound
event s nt df to = Map.alter change nt s
	where
	change = Just . mapLoudness' (\ml -> changeLoudness' ml df to)

sound :: Sound -> Int -> [(Int, (Doremi, Float, Float))] -> ([Float], Sound)
sound s n chs = sound' n s 0 chs

sound' :: Int -> Sound -> Int -> [(Int, (Doremi, Float, Float))] -> ([Float], Sound)
sound' m s n chs = case mp of
	Nothing -> ([], s')
	Just p -> (p :) `first` uncurry3 (sound' m) snchs
	where (mp, snchs@(s', _, _)) = sound1 m s n chs

foo :: Int -> (Sound, Int, [(Int, (Doremi, Float, Float))]) -> [Float]
foo m = L.unfoldr (uncurry3 $ sound1' m)
	where
	sound1' m s n chs = let (mp, (s', n', chs')) = sound1 m s n chs in
		case mp of
			Nothing -> Nothing
			Just p -> Just (p, (s', n', chs'))

sound1 :: Int -> Sound -> Int -> [(Int, (Doremi, Float, Float))] ->
	(Maybe Float, (Sound, Int, [(Int, (Doremi, Float, Float))]))
sound1 m s n chs
	| n >= m = (Nothing, (foldl event' s $ snd <$> chs, n + 1, []))
	| otherwise = let
		(s', chs') = maybe s (uncurry3 (event s)) `first` head' n chs
		(p, s'') = uncons' s' in
		(Just p, (s'',  n + 1, chs'))

head' :: Ord n => n -> [(n, a)] -> (Maybe a, [(n, a)])
head' _ [] = (Nothing, [])
head' n0 nxa@((n, x) : nxs)
	| n <= n0 = (Just x, nxs)
	| otherwise = (Nothing, nxa)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

event' s = uncurry3 (event s)

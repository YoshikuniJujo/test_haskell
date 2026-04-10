{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sound (

	Sound, PhaseLoudness, Event, zeroSound, sound, soundN, soundWhole

	) where

import Control.Arrow
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

event :: Sound -> Doremi -> Float -> Float -> Sound
event s nt df to = Map.alter change nt s
	where
	change = Just . mapLoudness' (\ml -> changeLoudness' ml df to)

soundN :: Sound -> Int -> [(Int, Event)] -> ([Float], Sound)
soundN s = (((fst3 `second`) .) $) . (`sound` s) . (const $) . (const .) . (>)

soundWhole :: [(Int, Event)] -> [Float]
soundWhole = fst . sound (\s _ chs -> not (null chs && Map.null s)) zeroSound

sound :: (Sound -> Int -> [(Int, Event)] -> Bool) -> Sound ->
	[(Int, Event)] -> ([Float], (Sound, Int, [(Int, Event)]))
sound p = ($ 0) . flip . curry3 . unfoldrWithSt . uncurry3 $ sound1' p

unfoldrWithSt :: (b -> (Maybe a, b)) -> b -> ([a], b)
unfoldrWithSt f st = let (mx, st') = f st in
	maybe ([], st') (\x -> (x :) `first` unfoldrWithSt f st') mx

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

type Event = (Doremi, Float, Float)

sound1' :: (Sound -> Int -> [(Int, Event)] -> Bool) ->
	Sound -> Int -> [(Int, Event)] ->
	(Maybe Float, (Sound, Int, [(Int, Event)]))
sound1' p s n chs
	| p s n chs = let
		(s', chs') = maybe s (uncurry3 (event s)) `first` head' n chs
		(sp, s'') = uncons' s' in
		(Just sp, (s'',  n + 1, chs'))
	| otherwise = (Nothing, (foldl event' s $ snd <$> chs, n, []))

head' :: Ord n => n -> [(n, a)] -> (Maybe a, [(n, a)])
head' _ [] = (Nothing, [])
head' n0 nxa@((n, x) : nxs)
	| n <= n0 = (Just x, nxs)
	| otherwise = (Nothing, nxa)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

event' :: Sound -> (Doremi, Float, Float) -> Sound
event' s = uncurry3 (event s)

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Graphics.UI.GLFW qualified as Glfw
import Linear (V2(..))
import KeyEvent
import PlaySound
import Hz qualified as Hz

main :: IO ()
main = do
	end <- atomically $ newTVar False
	la <- atomically $ newTVar Mu
	vkas <- withKeyActions 100 100 "Hello"
	snd <- atomically $ newTVar Sound {
		soundDoremi = La,
		soundPhase = 0,
		soundLoudness = Changing {
			changingNow = 0,
			changingDiff = 1 / 12000,
			changingTo = 0.7 } }
	playSound 0 48000 \(o :: MV.IOVector (V2 Float)) -> do
		kas <- atomically
			$ readTVar vkas <* writeTVar vkas []
		print kas
		atomically $ writeTVar la Mu
		for_ kas \k -> do
			atomically . when (keyActionKey k == Glfw.Key'Q) $ writeTVar end True
			atomically . when (keyActionKey k == Glfw.Key'K) $ writeTVar la La
			atomically . when (keyActionKey k == Glfw.Key'D) $ writeTVar la Do
			atomically . when (keyActionKey k == Glfw.Key'F) $ writeTVar la Re
			atomically . when (keyActionKey k == Glfw.Key'G) $ writeTVar la Mi
		s <- atomically $ readTVar snd
		print s
		let	(wf', s') = sound' s (MV.length o)
		for_ ([0 ..] `zip` wf') \(i, v) -> MV.write o i (V2 v v)
		atomically $ writeTVar snd s'

{-
		b <- atomically $ readTVar la
		print b
		if (b /= Mu)
		then do
			let	wf = case b of
					Do -> waveformDo
					Re -> waveformRe
					Mi -> waveformMi
					La -> waveformLa
				prd = round $ Hz.period case b of
					Do -> Hz.doo
					Re -> Hz.re
					Mi -> Hz.mi
					La -> Hz.la
			for_ [0 .. MV.length o - 1] \i -> do
				let	v = wf V.! (i `mod` prd)
					v' = v * size i
				MV.write o i $ V2 v' v'
		else for_ [0 .. MV.length o - 1] \i -> MV.write o i $ V2 0 0
		-}
		e <- atomically $ readTVar end
		pure $ if e then Complete else Continue

size :: Int -> Float
size i	| i < 12000 = 0.7 * fromIntegral i / 12000
	| i < 36000 = 0.7
	| i < 48000 = 0.7 * (48000 - fromIntegral i) / 12000
	| otherwise = 0

data Sound = Sound {
	soundDoremi :: Doremi,
	soundPhase :: Int,
	soundLoudness :: Loudness }
	deriving Show

sound :: Sound -> Int -> ([Float], Sound)
sound s n = go 0 $ soundLoudness s
	where
	go i l	| i == n = ([], Sound {
			soundDoremi = soundDoremi s,
			soundPhase = ph, soundLoudness = l })
		| otherwise = let
			v = wf V.! (ph `mod` prd)
			v' = currentLoudness l * v in (v' :) `first` go (i + 1) l'
		where
		ph = soundPhase s + i
		l' = nextLoudness l
	wf = case soundDoremi s of
		Do -> waveformDo
		Re -> waveformRe
		Mi -> waveformMi
		La -> waveformLa
	prd = round $ Hz.period case soundDoremi s of
		Do -> Hz.doo
		Re -> Hz.re
		Mi -> Hz.mi
		La -> Hz.la

sound' :: Sound -> Int -> ([Float], Sound)
sound' s n = go 0 $ soundLoudness s
	where
	go i l	| i == n = ([], Sound {
			soundDoremi = soundDoremi s,
			soundPhase = ph, soundLoudness = l })
		| otherwise = let
			v = wf V.! (ph `mod` prd)
			v' = currentLoudness l * v in (v' :) `first` go (i + 1) l'
		where
		ph = soundPhase s + i
		l' = if ph == 36000 then changeLoudness l (- 1 / 12000) 0 else nextLoudness l
	wf = case soundDoremi s of
		Do -> waveformDo
		Re -> waveformRe
		Mi -> waveformMi
		La -> waveformLa
	prd = round $ Hz.period case soundDoremi s of
		Do -> Hz.doo
		Re -> Hz.re
		Mi -> Hz.mi
		La -> Hz.la

data Loudness
	= Constant Float
	| Changing {
		changingNow :: Float,
		changingDiff :: Float,
		changingTo :: Float }
	deriving Show

currentLoudness :: Loudness -> Float
currentLoudness = \case
	(Constant l) -> l
	Changing { changingNow = l } -> l

nextLoudness :: Loudness -> Loudness
nextLoudness = \case
	l@(Constant _) -> l
	Changing {	changingNow = n,
			changingDiff = d,
			changingTo = t }
		| between n (n + d) t -> Constant t
		| otherwise -> Changing (n + d) d t

changeLoudness :: Loudness -> Float -> Float -> Loudness
changeLoudness l d t = case l of
	Constant n -> Changing {
		changingNow = n,
		changingDiff = d,
		changingTo = t }
	Changing { changingNow = n } -> Changing {
		changingNow = n,
		changingDiff = d,
		changingTo = t }

between :: Ord n => n -> n -> n -> Bool
between mn mx nw = mn <= nw && nw <= mx || mx <= nw && nw <= mn

data Doremi
	= Mu
	| Do | Re | Mi | Fa | So | La | Ti | HDo deriving (Show, Eq)

waveformDoremi :: Doremi -> V.Vector Float
waveformDoremi = \case
	Do -> waveformDo
	La -> waveformLa

waveformDo, waveformRe, waveformMi, waveformLa :: V.Vector Float
waveformDo = Hz.waveform Hz.doo
waveformRe = Hz.waveform Hz.re
waveformMi = Hz.waveform Hz.mi
waveformLa = Hz.waveform Hz.la

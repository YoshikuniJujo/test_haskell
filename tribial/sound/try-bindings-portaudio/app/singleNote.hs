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
import Doremi qualified as Doremi
import Hz qualified as Hz

import Loudness

main :: IO ()
main = do
	print =<< Glfw.getTime
	time <- atomically $ newTVar 0
	end <- atomically $ newTVar False
	la <- atomically $ newTVar Mu
	(vkas, _) <- withKeyActions 100 100 "Hello"
	{-
	snd <- atomically $ newTVar Sound {
		soundDoremi = La,
		soundPhase = 0,
		soundLoudness = Changing {
			changingNow = 0,
			changingDiff = 1 / 12000,
			changingTo = 0.7 } }
			-}
	snd <- atomically $ newTVar Sound {
		soundDoremi = La,
		soundPhase = 0,
		soundLoudness = Silent }

	print =<< Glfw.getTime

	doremi <- atomically $ newTVar La

	playSound 0 48000 \(o :: MV.IOVector (V2 Float)) -> do
		changers <- atomically $ newTVar []
		tm <- atomically $ readTVar time
		atomically . maybe (pure ()) (writeTVar time) =<< Glfw.getTime
		kas <- atomically
			$ readTVar vkas <* writeTVar vkas []
		print kas
		atomically $ writeTVar la Mu
		for_ kas \k -> do
			atomically . when (keyActionKey k == Glfw.Key'Q) $ writeTVar end True
			when (keyActionKey k == Glfw.Key'K) do
				atomically $ writeTVar doremi La
				print tm
				print k
				print $ keyActionToChanger tm k
				atomically $ modifyTVar changers (++ [keyActionToChanger tm k])
				pure ()
			when (keyActionKey k == Glfw.Key'D) do
				atomically $ writeTVar doremi Do
				print tm
				print k
				print $ keyActionToChanger tm k
				atomically $ modifyTVar changers (++ [keyActionToChanger tm k])
				pure ()
			when (keyActionKey k == Glfw.Key'F) do
				atomically $ writeTVar doremi Re
				print tm
				print k
				print $ keyActionToChanger tm k
				atomically $ modifyTVar changers (++ [keyActionToChanger tm k])
				pure ()
			when (keyActionKey k == Glfw.Key'G) do
				atomically $ writeTVar doremi Mi
				print tm
				print k
				print $ keyActionToChanger tm k
				atomically $ modifyTVar changers (++ [keyActionToChanger tm k])
				pure ()
		s <- atomically $ readTVar snd
		drm <- atomically $ readTVar doremi
		print s
		chngrs <- atomically $ readTVar changers
		let	-- (wf', s') = sound' s (MV.length o)
--			(wf', s') = singleNote s (MV.length o) [(24000, (- 1 / 96000, 0))]
			(wf', s') = singleNote s { soundDoremi = drm } (MV.length o) chngrs
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
				prd = round $ Doremi.period case b of
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

keyActionToChanger :: Double -> KeyAction -> (Int, (Float, Float))
keyActionToChanger tm0 KeyAction { keyActionTime = tm, keyActionAction = pr } = let
	n = (tm - tm0) * 48000
	dt = case pr of
		Press -> (1 / 12000, 0.7)
		Release -> (- 1 / 36000, 0) in (round n, dt)

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
	prd = round $ Doremi.period case soundDoremi s of
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
--		l' = if ph == 36000 then changeLoudness l (- 1 / 12000) 0 else nextLoudness l
		l' = if ph == 72000 then changeLoudness l (- 1 / 48000) 0 else nextLoudness l
	wf = case soundDoremi s of
		Do -> waveformDo
		Re -> waveformRe
		Mi -> waveformMi
		La -> waveformLa
	prd = round $ Doremi.period case soundDoremi s of
		Do -> Hz.doo
		Re -> Hz.re
		Mi -> Hz.mi
		La -> Hz.la

singleNote :: Sound -> Int -> [(Int, (Float, Float))] -> ([Float], Sound)
singleNote s n chs = go 0 (soundLoudness s) chs
	where
	go i l []
		| i == n = ([], Sound {
			soundDoremi = soundDoremi s,
			soundPhase = ph, soundLoudness = l })
		| otherwise = let
			v = wf V.! (ph `mod` prd)
			v' = currentLoudness l * v in (v' :) `first` go (i + 1) l' []
		where
		ph = soundPhase s + i
		l' = nextLoudness l
	go i l cha@((ci, dt) : chs)
		| i == n = ([], Sound {
			soundDoremi = soundDoremi s,
			soundPhase = ph, soundLoudness = l })
		| i == ci = let
			v = wf V.! (ph `mod` prd)
			l'' = uncurry (changeLoudness l) dt
			v' = currentLoudness l * v in (v' :) `first` go (i + 1) l'' chs
		| otherwise = let
			v = wf V.! (ph `mod` prd)
			v' = currentLoudness l * v in (v' :) `first` go (i + 1) l' cha
		where
		ph = soundPhase s + i
		l' = nextLoudness l
	wf = case soundDoremi s of
		Do -> waveformDo
		Re -> waveformRe
		Mi -> waveformMi
		La -> waveformLa
	prd = round $ Doremi.period case soundDoremi s of
		Do -> Hz.doo
		Re -> Hz.re
		Mi -> Hz.mi
		La -> Hz.la

data Doremi
	= Mu
	| Do | Re | Mi | Fa | So | La | Ti | HDo deriving (Show, Eq)

waveformDoremi :: Doremi -> V.Vector Float
waveformDoremi = \case
	Do -> waveformDo
	La -> waveformLa

waveformDo, waveformRe, waveformMi, waveformLa :: V.Vector Float
waveformDo = Doremi.hzWaveform Hz.doo
waveformRe = Doremi.hzWaveform Hz.re
waveformMi = Doremi.hzWaveform Hz.mi
waveformLa = Doremi.hzWaveform Hz.la

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import Data.Vector.Storable.Mutable qualified as MV
import System.Clock
import System.PortAudio
import Linear

import PlaySound
import Sound qualified as Sound
import Keyboard
import KeyboardSound

main :: IO ()
main = do
	time <- atomically $ newTVar 0
	sd <- atomically $ newTVar Sound.zeroSound
	c <- withKeyboard
	c' <- atomically $ dupTChan c
	void . forkIO . forever $ appendFile "keyboard.txt" . (++ "\n") . show =<< atomically (readTChan c')
	playSound 0 4800 \(o :: MV.IOVector (V2 Float)) -> do
		tm <- getTime Monotonic
		tm0 <- atomically $ readTVar time <* writeTVar time tm
		ns <- atomically (readTChanList c)
		print ns
		s <- atomically $ readTVar sd
		let	evs = noteToChanger tm0 `mapMaybe` ns
			(wf, s') = Sound.soundN s (MV.length o) evs
		print evs
		for_ ([0 ..] `zip` wf) \(i, v) -> do
			when (v < - 1 || v > 1) $ putStrLn "OVER"
			MV.write o i (V2 v v)
		atomically $ writeTVar sd s'
		let	volume0 = NoNote 176 7 0 `elem` (snd <$> ns)
		pure if volume0 then Complete else Continue

readTChanList :: TChan a -> STM [a]
readTChanList c =
	maybe (pure []) ((<$> readTChanList c) . (:)) =<< tryReadTChan c

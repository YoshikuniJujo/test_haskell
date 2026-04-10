{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Graphics.UI.GLFW qualified as Glfw
import Linear (V2(..))
import KeyEvent
import PlaySound
import Sound qualified as Sound

import KeySound

main :: IO ()
main = do
	time <- atomically $ newTVar 0
	sd <- atomically $ newTVar Sound.zeroSound
	print =<< Glfw.getTime
	(vkas, cka) <- withKeyActions 100 100 "Hello"
	void . forkIO . forever $ appendFile "tmp.txt" . keyLogToText =<< atomically (readTChan cka)
	playSound 0 4800 \(o :: MV.IOVector (V2 Float)) -> do
		mtm <- Glfw.getTime
		tm0 <- atomically do
			t <- readTVar time
			maybe (pure ()) (writeTVar time) mtm
			pure t
		print tm0
		kas <- atomically $ readTVar vkas <* writeTVar vkas []
		print kas
		s <- atomically $ readTVar sd
		let	evs = keyActionToChangers tm0 `concatMap` kas
			(wf, s') = Sound.soundN s (MV.length o) evs

		print evs
		for_ ([0 ..] `zip` wf) \(i, v) -> do
			when (v < - 1 || v > 1) $ putStrLn "OVER"
			MV.write o i (V2 v v)
		atomically $ writeTVar sd s'
		print s'
		print $ length wf

		pure if endKey kas then Complete else Continue
	where endKey = not . null . filter ((== Key'Q) . keyActionKey)

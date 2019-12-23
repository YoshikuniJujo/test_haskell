{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.Bool
import Graphics.Vty

import qualified Data.Map as M

import Draw
import State
import Tips

i, j, t :: [(Int, Int)]
i = [(- 1, 0) , (0, 0), (1, 0), (2, 0)]
j = [(- 1, - 1), (- 1, 0), (0, 0), (1, 0)]
l = [(- 1, 0), (0, 0), (1, 0), (1, - 1)]
o = [(0, 0), (1, 0), (0, 1), (1, 1)]
s = [(- 1, 1), (0, 1), (0, 0), (1, 0)]
t = [(- 1, 0), (0, - 1), (0, 0), (1, 0)]
z = [(- 1, 0), (0, 0), (0, 1), (1, 1)]

shapes :: [([(Int, Int)], Color)]
shapes = cycle [(i, red), (j, green), (l, yellow), (o, blue), (s, magenta), (t, cyan), (z, white)]

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	changed <- atomically $ newTVar False
	state <- atomically . newTVar $ State (4, 1) [(- 1, 0), (0, - 1), (0, 0), (1, 0)] cyan M.empty shapes 0
	forkForever do
		st <- atomically do
			bool retry (return ()) =<< readTVar changed
			writeTVar changed False
			readTVar state
		draw vty st
	void . forkIO $ forever do
		atomically do
			writeTVar changed True
			modifyTVar state moveDown
		threadDelay 250000
	loopIf do
		e <- nextEvent vty
		case e of
			EvKey KLeft [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> moveLeft s
				pure True
			EvKey (KChar 'h') [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> moveLeft s
				pure True
			EvKey KRight [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> moveRight s
				pure True
			EvKey (KChar 'l') [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> moveRight s
				pure True
			EvKey (KChar 'j') [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> rotateLeft s
				pure True
			EvKey (KChar 'k') [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> rotateRight s
				pure True
			EvKey (KChar ' ') [] -> do
				atomically do
					writeTVar changed True
					modifyTVar state \s -> moveBottom s
				pure True
			EvKey (KChar 'q') [] -> pure False
			_ -> pure True
	shutdown vty

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

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	changed <- atomically $ newTVar False
	state <- atomically . newTVar $ State (4, 0) M.empty
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
			EvKey (KChar 'q') [] -> pure False
			_ -> pure True
	shutdown vty

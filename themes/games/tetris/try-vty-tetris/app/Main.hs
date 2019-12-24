{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bool
import System.Random
import Graphics.Vty

import qualified Data.Map as M

import Draw
import State
import Tips

import Minos

shapes :: [(Mino, Color)]
shapes = randomCycle (mkStdGen 123) $ zip standardMinos [red, green, yellow, blue, magenta, cyan, white]

randomCycle :: StdGen -> [a] -> [a]
randomCycle gen lst = rc gen lst lst
	where
	rc g xs0 [] = rc g xs0 xs0
	rc g xs0 xs = let ((x, xs'), g') = randomPop g xs in x : rc g' xs0 xs'

randomPop :: StdGen -> [a] -> ((a, [a]), StdGen)
randomPop _ [] = error "bad"
randomPop g xs = ((xs !! i, take i xs ++ drop (i + 1) xs), g')
	where (i, g') = randomR (0, length xs - 1) g

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	changed <- atomically $ newTVar False
	state <- atomically . newTVar $ State (4, 1) (fst $ head shapes) (snd $ head shapes) M.empty (tail shapes) 0
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

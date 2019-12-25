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
	state <- atomically . newTVar $ State (4, 1) (fst $ head shapes) (snd $ head shapes) M.empty (tail shapes) 0 False 0 0 True
	dt <- forkIO $ forever  do
		st <- atomically do
			s <- readTVar state
			bool retry (return ()) $ pending s
			writeTVar state $ s { pending = False }
			pure s
		draw vty st
	void . forkIO $ forever do
		atomically do
			st <- readTVar state
			if pause st then retry else return ()
			modifyTVar state moveDown
		threadDelay 25000
	void . forkIO $ forever do
		atomically do
			st <- readTVar state
			if pause st then return () else retry
		e <- nextEvent vty
		case e of
			EvKey (KChar 'p') [] -> atomically $ modifyTVar state pauseGame
			_ -> return ()
	loopIf do
		atomically do
			st <- readTVar state
			if pause st then retry else return ()
		e <- nextEvent vty
		case e of
			EvKey KLeft [] -> do
				atomically do
					modifyTVar state \s -> moveLeft s
				pure True
			EvKey (KChar 'h') [] -> do
				atomically do
					modifyTVar state \s -> moveLeft s
				pure True
			EvKey KRight [] -> do
				atomically do
					modifyTVar state \s -> moveRight s
				pure True
			EvKey (KChar 'l') [] -> do
				atomically do
					modifyTVar state \s -> moveRight s
				pure True
			EvKey (KChar 'j') [] -> do
				atomically do
					modifyTVar state \s -> rotateLeft s
				pure True
			EvKey (KChar 'k') [] -> do
				atomically do
					modifyTVar state \s -> rotateRight s
				pure True
			EvKey (KChar ' ') [] -> do
				atomically do
					modifyTVar state \s -> moveBottom s
				pure True
			EvKey (KChar 'p') [] -> do
				atomically $ modifyTVar state pauseGame
				pure True
			EvKey (KChar 'q') [] -> pure False
			_ -> pure True
	killThread dt
	shutdown vty

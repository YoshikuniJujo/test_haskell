{-# LANGUAGE BlockArguments, PatternSynonyms, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bool
import System.Random
import Graphics.Vty

import Draw
import State
import Tips

import Minos

pattern KeyChar :: Char -> Event
pattern KeyChar c = EvKey (KChar c) []

shapes :: StdGen -> [(Mino, Color)]
shapes g = randomCycle g
	$ zip standardMinos [red, green, yellow, blue, magenta, cyan, white]

main :: IO ()
main = do
	vty <- mkVty =<< standardIOConfig
	state <- atomically . newTVar . initialState . shapes =<< getStdGen
	let	modifyState = atomically . modifyTVar state
	dt <- forkForever $ (draw vty =<<) . atomically $ readTVar state >>=
		\st -> st <$ do
			bool retry (return ()) $ pending st
			modifyTVar state drawed
	void $ forkForever do
		atomically $ bool retry (return ()) . pause =<< readTVar state
		nextEvent vty >>= \case
			KeyChar 'p' -> modifyState pauseGame
			_ -> return ()
	void . forkForever $ threadDelay 25000 <* atomically do
		bool (return ()) retry . pause =<< readTVar state
		modifyTVar state moveDown
	loopIf do
		atomically $ bool (return ()) retry . pause =<< readTVar state
		nextEvent vty >>= \case
			EvKey KLeft [] -> True <$ modifyState moveLeft
			EvKey KRight [] -> True <$ modifyState moveRight
			KeyChar 'h' -> True <$ modifyState moveLeft
			KeyChar 'l' -> True <$ modifyState moveRight
			KeyChar 'j' -> True <$ modifyState rotateLeft
			KeyChar 'k' -> True <$ modifyState rotateRight
			KeyChar ' ' -> True <$ modifyState moveBottom
			KeyChar 'p' -> True <$ modifyState pauseGame
			KeyChar 'q' -> pure False
			_ -> pure True
	killThread dt
	shutdown vty

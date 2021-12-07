{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM

import Human
import Human.Event
import Human.MainLoop
import Game

main :: IO ()
main = do
	gs <- atomically $ newTVar dummyGameState
	f <- fieldNew
	mainLoop \case
		EventEventTick _evt -> True <$ do
			gameDraw f =<< atomically do
				modifyTVar gs (`gameEvent` Tick)
				readTVar gs
		EventEventChar evc -> do
			print evc
			pure $ eventCharToCharacter evc /= 113
		_ -> pure True

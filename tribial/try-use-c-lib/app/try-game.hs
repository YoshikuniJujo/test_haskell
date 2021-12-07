{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (Either(..))

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
			case eventCharToCharacter evc of
				104 -> atomically
					$ modifyTVar gs (`gameEvent` Left)
				107 -> atomically
					$ modifyTVar gs (`gameEvent` Jump)
				_ -> pure ()
			pure $ eventCharToCharacter evc /= 113
		_ -> pure True

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
		EventEventTick _evt -> do
			gameDraw f =<< atomically do
				modifyTVar gs (`gameEvent` Tick)
				readTVar gs
			atomically $ not . doesGameFailure <$> readTVar gs
		EventEventChar evc -> do
			case eventCharToCharacter evc of
				104 -> atomically
					$ modifyTVar gs (`gameEvent` Left)
				106 -> atomically
					$ modifyTVar gs (`gameEvent` Stop)
				107 -> atomically
					$ modifyTVar gs (`gameEvent` Jump)
				108 -> atomically
					$ modifyTVar gs (`gameEvent` Right)
				_ -> pure ()
			pure $ eventCharToCharacter evc /= 113
		_ -> pure True
	s <- atomically $ readTVar gs
	if gameStateFailure s
		then putStrLn "Y O U   L O S E !"
		else putStrLn "B Y E !"

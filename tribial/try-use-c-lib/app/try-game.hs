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
	gs <- atomically $ newTVar initGameState
	f <- fieldNewBackgroundRaw False
	mainLoop \case
		EventEventTick _evt -> do
			gameDraw f =<< atomically do
				modifyTVar gs (`gameInput` Tick)
				readTVar gs
			atomically $ not . doesGameFailure <$> readTVar gs
		EventEventChar evc -> do
			case eventCharToCharacter evc of
				104 -> atomically
					$ modifyTVar gs (`gameInput` Left)
				106 -> atomically
					$ modifyTVar gs (`gameInput` Stop)
				107 -> atomically
					$ modifyTVar gs (`gameInput` Jump)
				108 -> atomically
					$ modifyTVar gs (`gameInput` Right)
				_ -> pure ()
			pure $ eventCharToCharacter evc /= 113
		_ -> pure True

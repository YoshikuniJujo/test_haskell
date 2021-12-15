{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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
	gsv <- atomically $ newTVar initGameState
	f <- fieldNewBackgroundRaw False
	mainLoop \case
		EventEventTick _evt -> do
			gs <- atomically do
				modifyTVar gsv (`gameInput` Tick); readTVar gsv
			not (gameStateFailure gs) <$ gameDraw f gs
		EventEventChar (eventCharToCharacter -> c) ->
			(c /= 113) <$ atomically case c of
				104 -> modifyTVar gsv (`gameInput` Left)
				106 -> modifyTVar gsv (`gameInput` Stop)
				107 -> modifyTVar gsv (`gameInput` Jump)
				108 -> modifyTVar gsv (`gameInput` Right)
				_ -> pure ()
		_ -> pure False

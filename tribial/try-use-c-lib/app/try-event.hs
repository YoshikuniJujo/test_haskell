{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Human.MainLoop
import Human.Event

main :: IO ()
main = mainLoop \case
	EventEventTick evt -> True <$ print evt
	EventEventChar evc -> do
		print evc
		pure (eventCharToCharacter evc /= 113)
	ev -> False <$ print ev

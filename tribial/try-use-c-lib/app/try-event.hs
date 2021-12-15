{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Human.MainLoop
import Human.Event

main :: IO ()
main = mainLoop \case
	EventEventTick evt -> True <$ print evt
	EventEventChar evc -> (eventCharToCharacter evc /= 113) <$ print evc
	ev -> False <$ print ev

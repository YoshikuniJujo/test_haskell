{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Exception
import Data.Bool
import System.IO

import Human.Event

main :: IO ()
main = do
	bfm <- hGetBuffering stdin
	finally (hSetBuffering stdin bfm) do
		hSetBuffering stdin NoBuffering
		ch <- hGetAndPushCChar stdin
		doWhile_ $ withEvent ch \case
			EventEventTick evt -> True <$ print evt
			EventEventChar evc -> do
				print evc
				pure (eventCharToCharacter evc /= 113)
			ev -> False <$ print ev

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

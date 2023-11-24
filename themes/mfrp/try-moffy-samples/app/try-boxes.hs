{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTChan, writeTChan)
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Run.Gtk4
import Data.Type.Set
import Data.OneOrMoreApp

import Trial.Boxes

main :: IO ()
main = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		interpret
			(retry $ handle @(Singleton DeleteEvent) Nothing er eo)
			v $ waitFor deleteEvent
		putStrLn "AFTER INTERPRET"
	runSingleWin eo

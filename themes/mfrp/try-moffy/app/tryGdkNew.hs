{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle
import Control.Moffy.Handle.DefaultWindow
import Data.Type.Set
import Data.Map (empty)
import Trial.TryGdk

import Graphics.Gdk.GdkDisplay

import Trial.Paper

main :: IO ()
main = do
	wid <- atomically . newTVar $ WindowId 0
	i2w <- atomically $ newTVar empty
	w2i <- atomically $ newTVar empty
	_ <- gdkDisplayOpen ""
	(print =<<) . ($ (Nothing :: Maybe WindowId)) $ interpretReactSt @_ @(WindowNew :- DefaultWindowEv :+: MouseDown :- 'Nil)
		(retrySt $ liftHandle' (handleGdk wid i2w w2i) `mergeSt` handleDefaultWindow) do
		w <- adjust windowNew
		adjust $ storeDefaultWindow w
		adjust sameClick

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Handle
import Control.Moffy.Event.Window
import Control.Moffy.Event.Mouse
import Data.Type.Set
import Data.Map (empty)
import Trial.TryGdk

import Graphics.Gdk.GdkDisplay

main :: IO ()
main = do
	wid <- atomically . newTVar $ WindowId 0
	i2w <- atomically $ newTVar empty
	w2i <- atomically $ newTVar empty
	_ <- gdkDisplayOpen ""
	print =<< interpretReact @_ @(WindowNew :- MouseDown :- 'Nil)
		(retry $ handleGdk wid i2w w2i) do
		w <- adjust windowNew
		adjust $ mouseDown w

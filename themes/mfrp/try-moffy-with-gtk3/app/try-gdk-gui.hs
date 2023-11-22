{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Control.Moffy.Event.Window
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Map

import Graphics.Gdk.GdkDisplay

import Trial.TryGdk.Gui
import Trial.TryWindowKey

main :: IO ()
main = do
	_ <- gdkDisplayOpen ""
	wid <- atomically . newTVar $ WindowId 0
	i2w <- atomically $ newTVar empty
	w2i <- atomically $ newTVar empty
	print =<< interpret (retry $ handleGdk wid i2w w2i) print tryWindowConfigure

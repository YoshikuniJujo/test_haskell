{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Time
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.DefaultWindow
import Data.Type.Set
import Data.Map (empty)
import Data.Time.Clock.TAI
import Data.Time.Clock.System
import Trial.TryGdk

import Graphics.Gdk.GdkDisplay

import Trial.Paper
import Trial.Boxes

data TryGdkState = TryGdkState {
	windowId :: Maybe WindowId,
	timeMode :: Mode,
	lastTime :: AbsoluteTime } deriving Show

instance DefaultWindowState TryGdkState where
	getDefaultWindow = windowId
	putDefaultWindow s wid = s { windowId = Just wid }
	
instance TimeState TryGdkState where
	getMode = timeMode
	putMode s m = s { timeMode = m }
	getLatestTime = lastTime
	putLatestTime s t = s { lastTime = t }

initTryGdkState :: AbsoluteTime -> TryGdkState
initTryGdkState = TryGdkState Nothing InitialMode

getTAITime :: IO AbsoluteTime
getTAITime = systemToTAITime <$> getSystemTime

main :: IO ()
main = do
	wid <- atomically . newTVar $ WindowId 0
	i2w <- atomically $ newTVar empty
	w2i <- atomically $ newTVar empty
	_ <- gdkDisplayOpen ""
	t <- getTAITime
	(print =<<) . ($ initTryGdkState t) $ interpretReactSt @_ @(WindowNew :- DefaultWindowEv :+: MouseDown :- TimeEv)
		(retrySt $ handleGdk' wid i2w w2i (0.1, ()) `mergeSt` handleDefaultWindow) do
		w <- adjust windowNew
		adjust $ storeDefaultWindow w
		adjust doubler

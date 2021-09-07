{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, TVar, newTVar, readTVar)
import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Event.Time
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Handle
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Viewable.Shape
import Data.Type.Flip
import Data.Type.Set
import Data.Map (Map, empty, (!))
import Data.Maybe
import Data.Time.Clock.TAI
import Data.Time.Clock.System
import Data.Color
import Trial.TryGdk

import Graphics.Cairo.Drawing.CairoT

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

-- import Trial.Paper
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
	(print =<<) . ($ initTryGdkState t) $ interpretSt @_ @(WindowNew :- DefaultWindowEv :+: MouseDown :- TimeEv)
		(retrySt $ handleGdk' wid i2w w2i (0.1, ()) `mergeSt` handleDefaultWindow) (uncurry $ showColor i2w) do
		w <- waitFor $ adjust windowNew
		waitFor . adjust $ storeDefaultWindow w
		(w ,) <$%> adjustSig cycleColor

showColor :: TVar (Map WindowId GdkWindow) -> WindowId -> BColor -> IO ()
showColor i2w i bc = do
	w <- (! i) <$> atomically (readTVar i2w)
	r <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w r \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr $ bColorToRgb bc
		cairoPaint cr

bColorToRgb :: BColor -> Rgb
bColorToRgb Red = fromJust $ rgbDouble 0.5 0 0
bColorToRgb Green = fromJust $ rgbDouble 0 0.5 0
bColorToRgb Blue = fromJust $ rgbDouble 0 0 0.5
bColorToRgb Yellow = fromJust $ rgbDouble 0.5 0.5 0
bColorToRgb Cyan = fromJust $ rgbDouble 0 0.5 0.5
bColorToRgb Magenta = fromJust $ rgbDouble 0.5 0 0.5
